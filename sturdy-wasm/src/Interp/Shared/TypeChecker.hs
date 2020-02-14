{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE BangPatterns #-}

module Interp.Shared.TypeChecker
    ( validateFunc
    ) where

import Prelude hiding (compare, const, id, fail)
import Data.Profunctor
import Data.String
import Data.Concrete.Error
import Text.Printf
import Control.Category hiding ((.))
import Control.Arrow
import Control.Arrow.State
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.Trans as Trans
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.Concrete.Failure
import Control.Lens hiding (Const, op)

import Types
import Syntax
import Interp.Shared.GenericInterpreter
import Control.Arrow.Wasm
import Control.Arrow.Transformer.Wasm
import Control.Arrow.Chain

newtype CheckerT c x y = CheckerT { runCheckerT :: c x y }
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail e,
        ArrowState s, ArrowWasm v, ArrowReader r)

deriving instance ArrowFix (c x y) => ArrowFix (CheckerT c x y)

instance ArrowRun c => ArrowRun (CheckerT c) where
    type Run (CheckerT c) x y = Run c x y
    run = Trans.run . runCheckerT

instance (ArrowChoice c, ArrowFail String c, ArrowWasm WasmType c, ArrowState (WasmState v) c, ArrowReader Bool c)
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

    -- If a fail occurs in the if branch, it gets ignored
    onExit = proc () -> do
        tys <- getVals -< ()
        fr <- getTopFr -< ()
        shouldExit <- ask -< ()
        mapA_ checkType -< zip tys (view frRty fr)
        if shouldExit then do
            setFrames -< []
        else
            returnA -< ()

    if_ f g = proc (ty, x, y) -> do
        checkType -< (ty, I32)
        frames <- getFrames -< ()
        (local f) -< (True, x)
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

validateFunc :: String -> [WasmType] -> WasmModule -> Either String [WasmType]
validateFunc name vs mdl = do
    let comp = proc () -> do
            loadModule -< mdl
            f <- getFunc -< name
            pushClos -< makeClos f vs
            (interp :: CheckerT
                            (WasmT WasmType
                                (FailureT String
                                    (ReaderT Bool
                                        (->)))) () [WasmType]) -< ()
    toEither (snd <$> (Trans.run comp) (False, (empty, ())))


-- testInterp :: (ArrowChoice c, ArrowFail e c, IsString e, ArrowReader Bool c) => c () ()
-- testInterp = proc () -> do
--     f <- ask -< ()
--     if f then do
--         fail -< "foo"
--     else do
--         local testInterp -< (True, ())
--         returnA -< ()

-- newtype TestT c x y = TestT {runTest :: c x y} deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail e,
--     ArrowState s, ArrowWasm v, ArrowReader r)

-- deriving instance ArrowFix (c x y) => ArrowFix (TestT c x y)

-- instance ArrowRun c => ArrowRun (TestT c) where
--     type Run (TestT c) x y = Run c x y
--     run = Trans.run . runTest

-- test = do
--     let comp = proc () -> do
--         (testInterp :: TestT (FailureT String (ReaderT Bool (->))) () ()) -< ()
--     Trans.run comp