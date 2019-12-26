{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}

module Interp.Shared.ConcreteInterpreter
    ( execFunc
    ) where

import Prelude hiding (compare, const, id, fail)
import Data.String
import Text.Printf
import Data.Profunctor
import Control.Category hiding ((.))
import Control.Arrow
import Control.Arrow.State
import Control.Arrow.Fail
import qualified Control.Arrow.Trans as Trans
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Concrete.Failure
import Control.Lens hiding (Const, op)
import Data.Concrete.Error

import Interp.Shared.GenericInterpreter
import Control.Arrow.Wasm
import Control.Arrow.Transformer.Wasm
import Control.Arrow.Chain
import Types
import Syntax
import Interp.Util

newtype ConcreteT c x y = ConcreteT { runConcreteT :: c x y }
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail e,
        ArrowState s, ArrowWasm v)

deriving instance ArrowFix (c x y) => ArrowFix (ConcreteT c x y)

instance (ArrowChoice c, ArrowFail String c, ArrowWasm WasmVal c)
    => IsVal WasmVal (ConcreteT c) where

    const = id

    binary = proc (ty, op, v1, v2) -> do
        checkType -< (v1, ty)
        checkType -< (v2, ty)
        case op of
            Add -> returnA -< v1 + v2
            Mul -> returnA -< v1 * v2

    unary = proc (ty, op, v) -> do
        checkType -< (v, ty)
        case op of
            Neg -> returnA -< boolToWasm (not (wasmToBool v))

    compare = proc (ty, op, v1, v2) -> do
        checkType -< (v1, ty)
        checkType -< (v2, ty)
        case op of
            Eql -> returnA -< boolToWasm (v1 == v2)

    br = proc n -> do
        vs <- getVals -< ()
        doN popFr -< ((), n)
        pushVals -< vs
        fr <- popFr -< ()
        case view frKind fr of
            BlockK   -> pushFr -< set frInstrs [] fr
            LoopK is -> pushFr -< set frInstrs is fr

    onExit = id

    if_ = proc (v, rtys, ifBr, elBr) -> if wasmToBool v
        then block -< (rtys, ifBr)
        else block -< (rtys, elBr)

    call = proc f -> do
        vs <- popNVals -< length (fuParams f)
        pushClos -< makeClos f vs

checkType :: (ArrowChoice c, ArrowFail e c, IsString e)
    => c (WasmVal, WasmType) ()
checkType = proc (v, ty) -> if ofType v ty
    then returnA -< ()
    else fail -< fromString $ printf
        "Expected type %s but got %s." (show ty) (show (getType v))

instance ArrowRun c => ArrowRun (ConcreteT c) where
    type Run (ConcreteT c) x y = Run c x y
    run = Trans.run . runConcreteT

execFunc :: ExecType
execFunc name vs mdl =
    let comp = proc () -> do
            loadModule -< mdl
            f <- getFunc -< name
            pushClos -< makeClos f vs
            interp -< ()
        compC = comp :: ConcreteT (WasmT WasmVal (FailureT String (->))) () [WasmVal]
    in toEither (snd <$> (Trans.run compC) (empty, ()))
