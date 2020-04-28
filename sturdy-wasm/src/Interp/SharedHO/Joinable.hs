{-# LANGUAGE FlexibleInstances #-}

module Interp.SharedHO.Joinable
where

import qualified Data.Map as M

class Joinable a where
    join :: a -> a -> a

instance Joinable () where
    join _ _ = ()

instance Joinable a => Joinable (M.Map String a) where
    join = M.unionWith join

instance Joinable a => Joinable [a] where
    join = zipWith join
