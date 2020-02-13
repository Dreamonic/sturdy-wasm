{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Interp.Shared.TypeChecker
    ( checkFunc
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
import Control.Arrow.Trans as Trans
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Concrete.Failure
import Control.Lens hiding (Const, op)

import Types
import Syntax
import Interp.Shared.GenericInterpreter
import Interp.Util (CheckType)
import Control.Arrow.Wasm
import Control.Arrow.Transformer.Wasm
import Control.Arrow.Chain

newtype CheckerT c x y = CheckerT { runCheckerT :: c x y }
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail e,
        ArrowState s, ArrowWasm v)

deriving instance ArrowFix (c x y) => ArrowFix (CheckerT c x y)

instance ArrowRun c => ArrowRun (CheckerT c) where
    type Run (CheckerT c) x y = Run c x y
    run = Trans.run . runCheckerT

checkType :: (ArrowChoice c, ArrowFail e c, IsString e, PrintfType e)
    => c (WasmType, WasmType) ()
checkType = proc (actTy, expTy) -> if actTy == expTy
    then returnA -< ()
    else fail -< printf "Expected type %s but got %s." (show expTy) (show actTy)

checkTypeList :: (ArrowChoice c, ArrowFail e c, IsString e, PrintfType e)
    => c ([WasmType], [WasmType]) ()
checkTypeList = proc (actTys, expTys) -> if actTys == expTys
    then returnA -< ()
    else fail -< printf "Expected types %s but got %s." (show expTys)
        (show actTys) -- the lists are actually reversed in wasm studio

checkTypeListWeak :: (ArrowChoice c, ArrowFail e c, IsString e, PrintfType e)
    => c ([WasmType], [WasmType]) ()
checkTypeListWeak = proc (actTys, expTys) ->
    if actTys == expTys || (length expTys < length actTys &&
            take (length expTys) actTys == expTys)
        then returnA -< ()
        else fail -< printf "Expected types %s but got %s." (show expTys)
            (show actTys)

branchWith :: (ArrowChoice c, ArrowFail String c, ArrowWasm WasmType c)
    => c ([WasmType], [WasmType]) () -> c Int ()
branchWith check = proc n -> do
    tys <- getVals -< ()
    fr <- getFrAt -< n
    let actTys = case view frKind fr of
            BlockK  -> view frRty fr
            LoopK _ -> []
    check -< (tys, actTys)

instance (ArrowChoice c, ArrowFail String c, ArrowWasm WasmType c)
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
        returnA -< I32

    localSet = proc (var, ty) -> do
        readTy <- readLocal -< var
        checkType -< (ty, readTy)

    br = branchWith checkTypeListWeak

    brIf = branchWith checkTypeList

    onExit = proc () -> do
        tys <- getVals -< ()
        fr <- getTopFr -< ()
        checkTypeList -< (tys, (view frRty fr))

    if_ f g = proc (ty, x, y) -> do
        checkType -< (ty, I32)
        f -< x
        g -< y -- TODO: this clearly isn't the correct implementation.

    call = proc f -> do
        tys <- popNVals -< length (fuParams f)
        checkTypeList -< (tys, reverse (prmType <$> fuParams f))
        pushVals -< (fuRty f)


enterFunc :: (ArrowWasm WasmType c, ArrowFail e c, IsString e, PrintfType e)
    => c (Func, [WasmType]) ()
enterFunc = proc (f, tys) -> do
    checkTypeList -< (tys, reverse (prmType <$> fuParams f))
    pushClos -< makeClos f tys

checkFunc :: CheckType
checkFunc name vs mdl =
    let comp = proc () -> do
            loadModule -< mdl
            f <- getFunc -< name
            enterFunc -< (f, vs)
            (interp :: CheckerT
                           (WasmT WasmType
                               (FailureT String
                                   (->))) () [WasmType]) -< ()
    in  toEither (snd <$> (Trans.run comp) (empty, ()))
