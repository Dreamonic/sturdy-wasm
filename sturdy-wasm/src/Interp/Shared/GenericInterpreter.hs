{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Interp.Shared.GenericInterpreter
    ( IsVal
    , const
    , binary
    , unary
    , compare
    , br
    , onExit
    , if_
    , call
    , interp
    ) where

import Prelude hiding (compare, const, id, fail)
import Data.String
import Control.Category
import Control.Arrow hiding (loop)
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Store

import Syntax
import Types
import Control.Arrow.Wasm

class Arrow c => IsVal v c | c -> v where
    const :: c WasmVal v
    binary :: c (WasmType, BinOpInstr, v, v) v
    unary :: c (WasmType, UnOpInstr, v) v
    compare :: c (WasmType, RelOpInstr, v, v) v
    br :: c Int ()
    onExit :: c () ()
    if_ :: c x z -> c y z -> c (v, x, y) z
    call :: c Func ()

type CanInterp v e c = (ArrowChoice c, IsVal v c, ArrowWasm v c, ArrowFail e c,
    IsString e)

interp :: CanInterp v e c => c () [v]
interp = proc () -> do
    next <- nextInstr -< ()
    case next of
        Just i  -> do
            interpInstr -< i
            interp -< ()
        Nothing -> do
            onExit -< ()
            vs <- popVals -< ()
            popFr -< ()
            continue <- hasFr -< ()
            if continue
                then do
                    pushVals -< vs
                    interp -< ()
                else do
                    popClos -< ()
                    continue2 <- hasClos -< ()
                    if continue2
                        then do
                            pushVals -< vs
                            interp -< ()
                        else returnA -< vs

interpInstr :: CanInterp v e c => c Instr ()
interpInstr = proc i -> case i of
    Const wv -> do
        v <- const -< wv
        pushVal -< v

    Block rtys is -> pushBlock -< (rtys, is)

    Loop rtys is -> pushLoop -< (rtys, is)

    Br n -> br -< n

    BrIf n -> do
        v <- popVal -< ()
        if_ br returnA -< (v, n, ())

    If rtys ifBr elBr -> do
        v <- popVal -< ()
        let r = error $ show (ifBr)
        if_ pushBlock pushBlock -< (v, (rtys, ifBr), (rtys, elBr))

    Call name -> do
        f <- getFunc -< name
        call -< f

    LocalGet var -> do
        v <- getLocal -< var
        pushVal -< v

    LocalSet var -> do
        v <- popVal -< ()
        setLocal -< (var, v)

    LocalTee var -> do
        putInstr -< LocalGet var
        putInstr -< LocalSet var

    Binary ty op -> do
        v1 <- popVal -< ()
        v2 <- popVal -< ()
        res <- binary -< (ty, op, v1, v2)
        pushVal -< res

    Unary ty op -> do
        v <- popVal -< ()
        res <- unary -< (ty, op, v)
        pushVal -< res

    Compare ty op -> do
        v1 <- popVal -< ()
        v2 <- popVal -< ()
        res <- compare -< (ty, op, v1, v2)
        pushVal -< res

    Nop -> id -< ()

    Unreachable -> fail -< fromString
        "Encountered unreachable instruction."
