{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Arrow.Wasm
    ( Frame(..)
    , frVStack
    , frIStack
    , frData
    , ArrowWasm
    , pushV
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

class (ArrowChoice a, Profunctor a) => ArrowWasm v fd a | a -> v, a -> fd where
    pushV :: a v ()
    popV :: a () v
    clearV :: a () ()
    pushF :: a (Frame v fd) ()
    popF :: a () (Frame v fd)
    setLocals :: a (M.Map String v) ()
    getLocal :: a String v
    setLocal :: a (String, v) ()
    setFuncs :: a (M.Map String Func) ()
    getFunc :: a String Func
