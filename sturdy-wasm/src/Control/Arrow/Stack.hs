{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Arrow.Stack 
( ArrowStack(..)
) where

import Control.Arrow
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowStack s c where
    push :: c s ()
    pop :: c () (Maybe s)
    -- isEmpty :: c () (Bool 