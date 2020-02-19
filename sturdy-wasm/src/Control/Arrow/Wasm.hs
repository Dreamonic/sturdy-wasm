{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

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
    , closExits
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
    , getDepth
    , addExitDepth
    , popMaxExitDepth
    , getMaxExitDepth
    ) where

import Prelude hiding (id)
import Data.Profunctor
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Category
import Control.Arrow
import Control.Lens.TH

import Syntax
import Types
import Control.Arrow.Chain

data FrameKind = BlockK | LoopK [Instr]

data Frame v = Frame { _frVals :: [v]
                     , _frInstrs :: [Instr]
                     , _frRty :: [WasmType]
                     , _frKind :: FrameKind }

blockFrame :: [WasmType] -> [Instr] -> Frame v
blockFrame rtys is = Frame [] is rtys BlockK

loopFrame :: [WasmType] -> [Instr] -> Frame v
loopFrame rtys is = Frame [] is rtys (LoopK is)

data Closure v = Closure { _closVars :: M.Map String v
                         , _closFrs :: [Frame v]
                         , _closExits :: S.Set Int }

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
    readLocal :: c String v
    writeLocal :: c (String, v) ()
    getFunc :: c String Func
    loadModule :: c WasmModule ()
    getExitDepths :: c () (S.Set Int)
    setExitDepths :: c (S.Set Int) ()
    simulate :: c x y -> c x y

pushBlock :: ArrowWasm v c => c ([WasmType], [Instr]) ()
pushBlock = proc (rtys, is) -> pushFr -< blockFrame rtys is

pushLoop :: ArrowWasm v c => c ([WasmType], [Instr]) ()
pushLoop = proc (rtys, is) -> pushFr -< loopFrame rtys is

makeClos :: Func -> [v] -> Closure v
makeClos f vs = let m = M.fromList $ zip (reverse (prmName <$> (fuParams f))) vs
                in  Closure m [blockFrame (fuRty f) (fuInstrs f)] S.empty

popVals :: ArrowWasm v c => c () [v]
popVals = proc () -> doWhile hasVal popVal -< ((), ())

popNVals :: ArrowWasm v c => c Int [v]
popNVals = (\n -> ((), n)) ^>> doN popVal >>^ reverse

pushVals :: ArrowWasm v c => c [v] ()
pushVals = reverse ^>> mapA_ pushVal

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

getDepth :: ArrowWasm v c => c () Int
getDepth = proc () -> do
    nonEmpty <- hasFr -< ()
    if nonEmpty
        then do
            fr <- popFr -< ()
            d <- getDepth -< ()
            pushFr -< fr
            returnA -< d + 1
        else returnA -< -1

addExitDepth :: ArrowWasm v c => c Int ()
addExitDepth = proc d -> do
    dSet <- getExitDepths -< ()
    setExitDepths -< S.insert d dSet

popMaxExitDepth :: ArrowWasm v c => c () Int
popMaxExitDepth = proc () -> do
    dSet <- getExitDepths -< ()
    case S.maxView dSet of
        Just (d, dSet') -> do
            setExitDepths -< dSet'
            returnA -< d
        Nothing         -> returnA -< -1

getMaxExitDepth :: ArrowWasm v c => c () Int
getMaxExitDepth = proc () -> do
    d <- popMaxExitDepth -< ()
    addExitDepth -< d
    returnA -< d
