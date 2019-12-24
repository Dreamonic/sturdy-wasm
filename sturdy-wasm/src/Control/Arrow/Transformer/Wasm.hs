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

data WasmState v = WasmState { _closures :: [Closure v]
                             , _funcs :: M.Map String Func }

makeLenses ''WasmState

newtype WasmT v c x y = WasmT (StateT (WasmState v) c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift,
              ArrowRun, ArrowFail e)

deriving instance (ArrowChoice c, Profunctor c)
    => ArrowState (WasmState v) (WasmT v c)

instance (ArrowChoice c, Profunctor c, ArrowFail e c, IsString e)
    => ArrowWasm v (WasmT v c) where
    pushVal   = modifyTopFrame $ proc (v, fr) ->
        returnA -< ((), over frVals (v:) fr)

    popVal    = modifyTopFrame $ proc ((), fr) -> do
        (v, vs') <- pop -< (fromString "Cannot pop from and empty value stack.",
            view frVals fr)
        returnA -< (v, set frVals vs' fr)

    pushFr    = modifyTopClos $ proc (fr, cl) ->
        returnA -< ((), over closFrs (fr:) cl)

    popFr     = modifyTopClos $ proc ((), cl) -> do
        (fr, frs') <- pop -< (fromString
            "Cannot pop from an empty frame stack.", view closFrs cl)
        returnA -< (fr, set closFrs frs' cl)

    pushClos  = modify $ proc (cl, st) ->
        returnA -< ((), over closures (cl:) st)

    popClos   = modify $ proc ((), st) -> do
        (cl, cls') <- pop -< (fromString
            "Cannot pop from an empty closure stack.", view closures st)
        returnA -< (cl, set closures cls' st)

    getLocal  = modifyTopClos $ proc (var, cl) -> do
        v <- lookup -< (fromString $ printf "Variable %s not in scope" var,
                        var, view closVars cl)
        returnA -< (v, cl)

    setLocal  = modifyTopClos $ proc ((var, v), cl) ->
        returnA -< ((), over closVars (M.insert var v) cl)

    setFuncs  = modify $ proc (newFuncs, st) ->
        returnA -< ((), set funcs newFuncs st)

    getFunc   = modify $ proc (name, st) -> do
        func <- lookup -< (fromString $ printf "No function %s in module" name,
                        name, view funcs st)
        returnA -< (func, st)

modifyTopFrame :: (ArrowChoice c, ArrowState (WasmState v) c, ArrowFail e c,
    IsString e) => c (x, Frame v) (y, Frame v) -> c x y
modifyTopFrame f = modifyTopClos $ proc (x, cl) -> case view closFrs cl of
    []      -> fail -< fromString "Tried to edit an empty frame stack."
    fr:frs  -> do
        (y, fr') <- f -< (x, fr)
        returnA -< (y, set closFrs (fr':frs) cl)

modifyTopClos :: (ArrowChoice c, ArrowState (WasmState v) c, ArrowFail e c,
    IsString e) => c (x, Closure v) (y, Closure v) -> c x y
modifyTopClos f = modify $ proc (x, st) -> case view closures st of
    []     -> fail -< fromString "Tried to edit an empty closure stack."
    cl:cls -> do
        (y, cl') <- f -< (x, cl)
        returnA -< (y, set closures (cl':cls) st)

pop :: (ArrowChoice c, ArrowFail e c, IsString e)
    => c (e, [a]) (a, [a])
pop = proc (msg, list) -> case list of
    []   -> fail -< msg
    x:xs -> returnA -< (x, xs)

lookup :: (ArrowChoice c, ArrowFail e c, IsString e, Ord k,
    Show k) => c (e, k, M.Map k v) v
lookup = proc (msg, k, mp) -> case M.lookup k mp of
        Just v  -> returnA -< v
        Nothing -> fail -< msg
