{-# LANGUAGE FlexibleInstances #-}

module Interp.SharedHO.Data.Joinable
where

import qualified Data.Map as M

class Joinable a where
    join :: a -> a -> a

instance Joinable () where
    join _ _ = ()

instance Joinable a => Joinable (M.Map String a) where
    join = M.unionWith join

instance Joinable a => Joinable [a] where
    join = zipFullWith join

zipFullWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zipFullWith f xs ys =
    let (shorter, longer) = if length xs <= length ys then (xs,ys) else (ys,xs)
    in  zipWith f shorter longer ++ drop (length shorter) longer
