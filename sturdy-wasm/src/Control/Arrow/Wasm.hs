{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}

module Control.Arrow.Wasm
    ( FrameKind(..)
    , Frame(Frame)
    , frVals
    , frInstrs
    , frRty
    , frKind
    , Closure
    , closVars
    , closFrs
    , ArrowWasm(..)
    , pushBlock
    , pushLoop
    , makeClos
    , popVals
    , popNVals
    , pushVals
    , getVals
    , getFrAt
    , getTopFr
    ) where

import Prelude hiding (id)
import Control.Category
import Control.Arrow
import Data.Profunctor
import qualified Data.Map as M
import Control.Lens.TH

import Syntax
import Types
import Control.Arrow.Chain

data FrameKind = BlockK | LoopK [Instr] deriving Show

data Frame v = Frame { _frVals :: [v]
                     , _frInstrs :: [Instr]
                     , _frRty :: [WasmType]
                     , _frKind :: FrameKind } deriving Show

blockFrame :: [WasmType] -> [Instr] -> Frame v
blockFrame rtys is = Frame [] is rtys BlockK

loopFrame :: [WasmType] -> [Instr] -> Frame v
loopFrame rtys is = Frame [] is rtys (LoopK is)

data Closure v = Closure { _closVars :: M.Map String v
                         , _closFrs :: [Frame v] }

makeLenses ''Frame
makeLenses ''Closure

class (ArrowChoice c, Profunctor c) => ArrowWasm v c | c -> v where
    pushVal :: c v ()
    popVal :: c () v
    hasVal :: c () Bool
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
    getFunc :: c String Func
    loadModule :: c WasmModule ()

pushBlock :: ArrowWasm v c => c ([WasmType], [Instr]) ()
pushBlock = proc (rtys, is) -> do
    pushFr -< blockFrame rtys is

pushLoop :: ArrowWasm v c => c ([WasmType], [Instr]) ()
pushLoop = proc (rtys, is) -> pushFr -< loopFrame rtys is

makeClos :: Func -> [v] -> Closure v
makeClos f vs = let m = M.fromList $ zip (getName <$> (fuParams f)) vs
                in  Closure m [blockFrame (fuRty f) (fuInstrs f)]

popVals :: ArrowWasm v c => c () [v]
popVals = proc () -> doWhile hasVal popVal -< ((), ())

popNVals :: ArrowWasm v c => c Int [v]
popNVals = (\n -> ((), n)) ^>> doN popVal

pushVals :: ArrowWasm v c => c [v] ()
pushVals = mapA_ pushVal

getVals :: ArrowWasm v c => c () [v]
getVals = proc () -> do
    vs <- popVals -< ()
    pushVals -< vs
    returnA -< vs

getFrAt :: ArrowWasm v c => c Int (Frame v)
getFrAt = proc n -> do
    frs <- doN popFr -< ((), n)
    targetFr <- popFr -< ()
    pushFr -< targetFr
    mapA_ pushFr -< frs
    returnA -< targetFr

getTopFr :: ArrowWasm v c => c () (Frame v)
getTopFr = (\_ -> 0) ^>> getFrAt
