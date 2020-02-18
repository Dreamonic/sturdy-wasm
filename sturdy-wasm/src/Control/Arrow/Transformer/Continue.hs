{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Arrow.Transformer.Continue
( ContinueT
) where

import Control.Category
import Control.Arrow
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Fail
import Control.Arrow.Transformer.State
import Data.Profunctor

import Control.Arrow.Continue

newtype ContinueT comp c x y = ContinueT (StateT comp c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, 
              ArrowRun, ArrowFail e)

deriving instance (ArrowChoice c, Profunctor c)
    => ArrowState comp (ContinueT comp c)

instance (Arrow c, ArrowChoice c, Profunctor c, Arrow comp, Profunctor comp) 
    => ArrowContinue comp (ContinueT (comp () ()) c) where
    continue = get
    store = put