{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Arrow.Transformer.Stack
( StackT
) where

import GHC.Exts(IsString(..))
import Prelude hiding (fail)
import Data.Profunctor
import Control.Category
import Control.Arrow
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Fail
import Control.Arrow.Transformer.State

import Control.Arrow.Stack

newtype StackT s c x y = StackT { runStack :: (StateT [s] c) x y }
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, 
              ArrowRun, ArrowFail e, ArrowState [s])

instance (Arrow c, ArrowChoice c, Profunctor c) 
    => ArrowStack s (StackT s c) where
    
    push = proc v -> do
        s <- get -< ()
        put -< v:s

    pop = proc () -> do
        s <- get -< ()
        case s of
            [] -> returnA -< Nothing
            v:vs -> do
                put -< vs
                returnA -< Just v

    -- isEmpty = proc () -> do
    --     s <- get -< ()
    --     arr null -< s
