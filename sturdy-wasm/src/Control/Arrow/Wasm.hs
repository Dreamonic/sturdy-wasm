{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Control.Arrow.Wasm
    ( Frame(..)
    , frVStack
    , frIStack
    , frData
    , ArrowWasm
    , pushV
    , pushManyV
    , popV
    , clearV
    , pushF
    , popF
    , setLocals
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

data Frame v fd = Frame {_frVStack :: [v], _frIStack :: [Instr], _frData :: fd}

makeLenses ''Frame

class (ArrowChoice c, Profunctor c) => ArrowWasm v fd c | c -> v, c -> fd where
    pushV :: c v ()
    pushManyV :: c [v] ()
    pushManyV = proc x -> case x of
        []   -> returnA -< ()
        v:vs -> do pushV -< v
                   pushManyV -< vs
    popV :: c () v
    clearV :: c () ()
    pushF :: c (Frame v fd) ()
    popF :: c () (Frame v fd)
    setLocals :: c (M.Map String v) ()
    getLocal :: c String v
    setLocal :: c (String, v) ()
    setFuncs :: c (M.Map String Func) ()
    getFunc :: c String Func
