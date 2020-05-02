{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}

module Interp.SharedHO.Data.RDSet
where

import qualified Data.Set as S
import qualified Data.Map as M

import Interp.SharedHO.Data.Joinable
import Interp.SharedHO.Data.BoolVal

data Set a = Top | Mid (S.Set a) deriving Eq

fromSet :: S.Set a -> Set a
fromSet s = if S.size s > 10 then Top else Mid s

singleton :: a -> Set a
singleton v = fromSet $ S.singleton v

instance Show a => Show (Set a) where
    show rds = case rds of
        Mid s -> "{" ++ (init . tail $ show $ S.toList s) ++ "}"
        Top   -> "⊤"

instance Ord a => Joinable (Set a) where
    join (Mid s1) (Mid s2) = fromSet $ s1 `S.union` s2
    join _ _ = Top

instance FromBool a => FromBool (Set a) where
    fromBool b = singleton $ fromBool b

add :: (Ord a, Num a) => Set a -> Set a -> Set a
add (Mid s1) (Mid s2) = fromSet $ S.map (\(x, y) -> x + y)
    (S.cartesianProduct s1 s2)
add _ _ = Top

eqz :: (Ord a, Num a, FromBool a) => Set a -> Set a
eqz (Mid s) = fromSet $ S.map (\n -> fromBool (n == 0)) s
eqz _ = Top

lt :: (Ord a, FromBool a) => Set a -> Set a -> Set a
lt (Mid s1) (Mid s2) =
    let lts = S.map (\(x, y) -> x < y) (S.cartesianProduct s1 s2)
        t   = fromBool True
        f   = fromBool False
    in  if
        | foldl (&&) True lts  -> t
        | foldl (||) False lts -> t `join` f
        | otherwise            -> f
lt _ _ = Top

if_ :: (Ord a, FromBool a, Joinable b) => Set a -> b -> b -> b
if_ (Mid s) t f = if
    | S.notMember (fromBool False) s -> t
    | S.size s == 1                  -> f
    | otherwise                      -> t `join` f
if_ _ t f = t `join` f
