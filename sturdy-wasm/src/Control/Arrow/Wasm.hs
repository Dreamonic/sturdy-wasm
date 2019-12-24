{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Control.Arrow.Wasm
    ( Frame(..)
    , frVals
    , frInstrs
    , frRty
    , frKind
    , Closure
    , closVars
    , closFrs
    , ArrowWasm
    , pushVal
    , popVal
    , getVals
    , nextInstr
    , putInstr
    , pushFr
    , popFr
    , hasFr
    , pushClos
    , popClos
    , hasClos
    , getLocal
    , setLocal
    , setFuncs
    , getFunc
    , block
    , loop
    ) where

import Control.Arrow hiding (loop)
import Data.Profunctor
import qualified Data.Map as M
import Control.Lens.TH

import Syntax
import Types

data FrameKind = BlockK | LoopK [Instr]

data Frame v = Frame { _frVals :: [v]
                     , _frInstrs :: [Instr]
                     , _frRty :: [WasmType]
                     , _frKind :: FrameKind }

data Closure v = Closure { _closVars :: M.Map String v
                         , _closFrs :: [Frame v] }

makeLenses ''Frame
makeLenses ''Closure

class (ArrowChoice c, Profunctor c) => ArrowWasm v c | c -> v where
    pushVal :: c v ()
    popVal :: c () v
    getVals :: c () [v]
    nextInstr :: c () (Maybe Instr)
    putInstr :: c Instr ()
    pushFr :: c (Frame v) ()
    popFr :: c () (Frame v)
    hasFr :: c () Bool
    pushClos :: c (Closure v) ()
    popClos :: c () (Closure v)
    hasClos :: c () Bool
    getLocal :: c String v
    setLocal :: c (String, v) ()
    setFuncs :: c (M.Map String Func) ()
    getFunc :: c String Func

block :: ArrowWasm v c => c ([WasmType], [Instr]) ()
block = proc (rtys, is) -> pushFr -< (Frame [] is rtys BlockK)

loop :: ArrowWasm v c => c ([WasmType], [Instr]) ()
loop = proc (rtys, is) -> pushFr -< (Frame [] is rtys (LoopK is))
