{-# LANGUAGE FlexibleInstances #-}

module Interp.SharedHO.Data.Widening
where

import qualified Data.Map as M
import qualified Data.Set as S
import Interp.SharedHO.Data.Joinable (zipFullWith)

class StdWideningSet a where
    stdWSet :: S.Set a

class Widening a where
    widening :: a -> a -> a

instance Widening a => Widening (M.Map String a) where
    widening = M.unionWith widening

instance Widening a => Widening [a] where
    widening = zipFullWith widening
