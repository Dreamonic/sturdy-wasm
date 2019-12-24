{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Interp.Shared.GenericInterpreter
(
) where

import Prelude hiding (compare, const, id, fail)
import Data.String
import Control.Category
import Control.Arrow hiding (loop)
import Control.Arrow.Trans
import Control.Arrow.Fail
import Control.Arrow.Fix

import Syntax
import Types
import Control.Arrow.Wasm

class Arrow c => IsVal v c | c -> v where
    const :: c WasmVal v
    binary :: c (WasmType, BinOpInstr, v, v) v
    unary :: c (WasmType, UnOpInstr, v) v
    compare :: c (WasmType, RelOpInstr, v, v) v
    br :: c Integer ()
    onExit :: c () ()
    if_ :: c (v, [WasmType], [Instr], [Instr]) ()
    call :: c Func ()

type CanInterp v e c = (ArrowChoice c, IsVal v c, ArrowWasm v c, ArrowFail e c,
    IsString e, ArrowFix (c () [v]))

interp :: CanInterp v e c => c () [v]
interp = fix $ \interp' -> proc () -> do
    next <- nextInstr -< ()
    case next of
        Just i  -> do
            step -< i
            interp' -< ()
        Nothing -> do
            tryFr <- popFr -< ()
            case tryFr of
                Just _  -> interp' -< ()
                Nothing -> do
                    tryClos <- popClos -< ()
                    case tryClos of
                        Just _ -> interp' -< ()
                        Nothing -> getVals -< ()

step :: CanInterp v e c => c Instr ()
step = proc i -> case i of
    Const wv -> do
        v <- const -< wv
        pushVal -< v

    Block rtys is -> block -< (rtys, is)

    Loop rtys is -> loop -< (rtys, is)

    Br n -> br -< n

    BrIf n -> putInstr -< If [] [Br n] []

    If rtys ifBr elBr -> do
        v <- popVal -< ()
        if_ -< (v, rtys, ifBr, elBr)

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
        putInstr -< LocalSet var
        putInstr -< LocalGet var

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
