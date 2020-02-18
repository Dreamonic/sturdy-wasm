{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Arrow.Continue 
( ArrowContinue(..)
) where

import Control.Arrow
import Data.Profunctor

class (Arrow c, Profunctor c, Arrow comp, Profunctor comp) => ArrowContinue comp c where

    -- | Executes the stored arrow if there is one
    continue :: c () (comp () ())

    -- | Store an arrow for later execution
    store :: c (comp () ()) ()