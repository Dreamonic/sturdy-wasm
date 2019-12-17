{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Control.Arrow.Wasm
    ( Frame(..)
    , frVals
    , frInstrs
    , frData
    , Closure
    , closVars
    , closFrs
    , ArrowWasm
    , pushVal
    , pushVals
    , popVal
    , pushFr
    , popFr
    , pushClos
    , popClos
    , getLocal
    , setLocal
    , setFuncs
    , getFunc
    ) where

import Control.Arrow
import Data.Profunctor
import qualified Data.Map as M
import Control.Lens.TH

import Syntax

data Frame v fd = Frame { _frVals :: [v]
                        , _frInstrs :: [Instr]
                        , _frData :: fd }
data Closure v fd = Closure { _closVars :: M.Map String v
                            , _closFrs :: [Frame v fd] }

makeLenses ''Frame
makeLenses ''Closure

class (ArrowChoice c, Profunctor c) => ArrowWasm v fd c | c -> v, c -> fd where
    pushVal :: c v ()
    pushVals :: c [v] ()
    pushVals = proc x -> case x of
        []   -> returnA -< ()
        v:vs -> do pushVal -< v
                   pushVals -< vs
    popVal :: c () v
    pushFr :: c (Frame v fd) ()
    popFr :: c () (Frame v fd)
    pushClos :: c (Closure v fd) ()
    popClos :: c () (Closure v fd)
    getLocal :: c String v
    setLocal :: c (String, v) ()
    setFuncs :: c (M.Map String Func) ()
    getFunc :: c String Func
