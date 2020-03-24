{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}

module Interp.SharedHO.RDSet
where

import qualified Data.Set as S
import qualified Data.Map as M

import Interp.SharedHO.Joinable

data Set a = Top | Mid (S.Set a) deriving (Eq, Show)

fromSet :: S.Set a -> Set a
fromSet s = if S.size s > 10 then Top else Mid s

singleton :: a -> Set a
singleton v = fromSet $ S.singleton v

instance Ord a => Joinable (Set a) where
    join (Mid s1) (Mid s2) = fromSet $ s1 `S.union` s2
    join _ _ = Top

add :: (Ord a, Num a) => Set a -> Set a -> Set a
add (Mid s1) (Mid s2) = fromSet $ S.map (\(x, y) -> x + y)
    (S.cartesianProduct s1 s2)
add _ _ = Top

if_ :: (Ord a, Num a, Joinable b) => Set a -> b -> b -> b
if_ (Mid s) t f = if
    | S.notMember 0 s -> f
    | S.size s == 1   -> t
    | otherwise       -> t `join` f
if_ _ t f = t `join` f

instance Ord v => Joinable (M.Map String (Set v)) where
    join st1 st2 = M.unionWith join st1 st2
