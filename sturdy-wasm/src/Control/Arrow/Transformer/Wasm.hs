{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Arrow.Transformer.Wasm
( WasmT(..)
) where

import qualified Data.Map as M
import Prelude hiding (fail, lookup)
import Data.String
import Text.Printf
import Data.Profunctor
import Control.Lens
import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Fail
import Control.Arrow.State
import Control.Arrow.Transformer.State

import Syntax
import Control.Arrow.Wasm

data WasmState v fd = WasmState { _vStack :: [v]
                                , _fStack :: [Frame v fd]
                                , _locals :: M.Map String v
                                , _funcs :: M.Map String Func }

makeLenses ''WasmState

newtype WasmT v fd c x y = WasmT (StateT (WasmState v fd) c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift,
              ArrowRun, ArrowFail e)

deriving instance (ArrowChoice c, Profunctor c)
    => ArrowState (WasmState v fd) (WasmT v fd c)

instance (ArrowChoice c, Profunctor c, ArrowFail e c, IsString e)
    => ArrowWasm v fd (WasmT v fd c) where
    pushV     = push $ over vStack
    popV      = pop (view vStack) (set vStack)
        "Cannot pop from an empty value stack."
    clearV    = modify $ proc ((), st) -> returnA -< ((), set vStack [] st)
    pushF     = push $ over fStack
    popF      = pop (view fStack) (set fStack)
        "Cannot pop from an empty frame stack."
    setLocals = replace $ set locals
    getLocal  = lookup (view locals) $ printf "Variable %s not in scope."
    setLocal  = modify $ proc ((var, v), st) ->
        returnA -< ((), over locals (M.insert var v) st)
    setFuncs  = replace $ set funcs
    getFunc   = lookup (view funcs) $ printf "No function %s in module."

push :: ArrowState s c => (([a] -> [a]) -> s -> s) -> c a ()
push overlens = modify $ proc (x, st) -> returnA -< ((), overlens (x:) st)

pop :: (ArrowChoice c, ArrowState s c, ArrowFail e c, IsString e)
    => (s -> [a]) -> ([a] -> s -> s) -> String -> c () a
pop viewlens setlens msg = modify $ proc ((), st) -> case viewlens st of
    []   -> fail -< fromString msg
    x:xs -> returnA -< (x, setlens xs st)

replace :: (ArrowState s c) => (a -> s -> s) -> c a ()
replace setlens = modify $ proc (new, st) -> returnA -< ((), setlens new st)

lookup :: (ArrowChoice c, ArrowState s c, ArrowFail e c, IsString e, Ord k,
    Show k) => (s -> M.Map k v) -> (String -> String) -> c k v
lookup viewlens msgF = proc k -> do
    st <- get -< ()
    case M.lookup k (viewlens st) of
        Just v  -> returnA -< v
        Nothing -> fail -< fromString $ msgF (show k)
