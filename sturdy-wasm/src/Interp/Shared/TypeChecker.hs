{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Interp.Shared.TypeChecker
    ( validateFunc
    ) where

import Prelude hiding (compare, const, id, fail)
import Data.Profunctor
import Data.String
import Data.HashMap.Strict
import Data.Concrete.Error
import Text.Printf
import Control.Category hiding ((.))
import Control.Arrow
import Control.Arrow.State
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.Store
import Control.Arrow.Trans as Trans
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.Concrete.Store
import Control.Arrow.Transformer.Concrete.Failure
import Control.Lens hiding (Const, op)

import Types
import Syntax
import Interp.Shared.GenericInterpreter
import Control.Arrow.Wasm
import Control.Arrow.Chain
import Control.Arrow.Stack
import Control.Arrow.Transformer.Wasm
import Control.Arrow.Transformer.Stack

import Parsing.Parser

newtype CheckerT c x y = CheckerT { runCheckerT :: c x y }
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail e,
        ArrowState s, ArrowWasm v, ArrowStack st)

deriving instance ArrowFix (c x y) => ArrowFix (CheckerT c x y)

instance ArrowRun c => ArrowRun (CheckerT c) where
    type Run (CheckerT c) x y = Run c x y
    run = Trans.run . runCheckerT

instance (ArrowChoice c, ArrowFail String c, ArrowWasm WasmType c, 
    ArrowState (WasmState v) c, ArrowStack SavedExecutionState c)
    => IsVal WasmType (CheckerT c) where

    const = arr getType

    binary = proc (opTy, _, ty1, ty2) -> do
        checkType -< (ty1, opTy)
        checkType -< (ty2, opTy)
        returnA -< opTy

    unary = proc (opTy, _, ty) -> do
        checkType -< (ty, opTy)
        returnA -< opTy

    compare = proc (opTy, _, ty1, ty2) -> do
        checkType -< (ty1, opTy)
        checkType -< (ty2, opTy)
        returnA -< opTy

    br = proc n -> do
        tys <- getVals -< ()
        fr <- getFrAt -< n
        mapA_ checkType -< zip tys (view frRty fr)

    onExit = proc () -> do
        tys <- getVals -< ()
        fr <- getTopFr -< ()
        mapA_ checkType -< zip tys (view frRty fr)
        exec <- pop -< ()
        case exec of
            Just exec' -> pushBlock -< exec'
            Nothing -> returnA -< ()

    if_ f g = proc (ty, x, y) -> do
        checkType -< (ty, I32)
        -- frames <- getFrames -< ()
        case getR y of
            Nothing -> do
                f -< x
                
            Just st -> do
                push -< st
                f -< x
        -- setFrames -< frames
        -- g -< y -- TODO: this clearly isn't the correct implementation.

    call = proc f -> do
        tys <- popNVals -< length (fuParams f)
        mapA_ checkType -< zip tys (fuRty f)


checkType :: (ArrowChoice c, ArrowFail e c, IsString e, PrintfType e)
    => c (WasmType, WasmType) ()
checkType = proc (actTy, expTy) -> if actTy == expTy
    then returnA -< ()
    else fail -< printf "Expected type %s but got %s." (show expTy) (show actTy)

type SavedExecutionState = ([WasmType], [Instr])

-- validateFunc :: String -> [WasmType] -> WasmModule -> Either String [WasmType]
validateFunc name vs mdl = do
    let comp = proc () -> do
            loadModule -< mdl
            f <- getFunc -< name
            pushClos -< makeClos f vs
            (interp :: CheckerT
                            (WasmT WasmType
                                (StackT SavedExecutionState
                                    (FailureT String
                                        (->)))) () [WasmType]) -< ()
    let result = (Trans.run comp) ([], (Control.Arrow.Transformer.Wasm.empty, ()))
    toEither (snd <$> (snd <$> result))
