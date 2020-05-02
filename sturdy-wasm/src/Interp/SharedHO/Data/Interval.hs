{-# LANGUAGE MultiWayIf #-}

module Interp.SharedHO.Data.Interval
where

import qualified Data.Set as S

import Interp.SharedHO.Data.BoolVal
import Interp.SharedHO.Data.Joinable
import Interp.SharedHO.Data.Widening

data Interval a = Interval { lowB :: a, highB :: a } deriving Eq

degenerate :: a -> Interval a
degenerate x = Interval x x

instance Show a => Show (Interval a) where
    show (Interval l h) = "[" ++ show l ++ ", " ++ show h ++ "]"

instance FromBool a => FromBool (Interval a) where
    fromBool b = degenerate $ fromBool b

instance Ord a => Joinable (Interval a) where
    (Interval l1 h1) `join` (Interval l2 h2) = Interval (min l1 l2) (max h1 h2)

instance (StdWideningSet a, Ord a) => Widening (Interval a) where
    (Interval l1 h1) `widening` (Interval l2 h2) =
        let helper i pick = if S.member i stdWSet
                                then i
                                else pick $ S.split i stdWSet
            leastLowInB i = helper i $ S.findMax . fst
            leastUpInB i  = helper i $ S.findMin . snd
            l3            = if l1 <= l2 then l1 else leastLowInB l2
            h3            = if h1 >= h2 then h1 else leastUpInB h2
        in  Interval l3 h3

add :: Num a => Interval a -> Interval a -> Interval a
add (Interval l1 h1) (Interval l2 h2) = Interval (l1 + l2) (h1 + h2)

eqz :: (Ord a, Num a, FromBool a) => Interval a -> Interval a
eqz (Interval l h) =
    let t = fromBool True
        f = fromBool False
    in  if
        | l == 0 && h == 0 -> t
        | l /= 0 && h /= 0 -> f
        | otherwise        -> t `join` f

lt :: (Ord a, FromBool a) => Interval a -> Interval a -> Interval a
lt (Interval l1 h1) (Interval l2 h2) =
    let t = fromBool True
        f = fromBool False
    in  if
        | l1 < l2 && h1 < h2   -> t
        | l1 >= l2 && h1 >= h2 -> f
        | otherwise            -> t `join` f

if_ :: (Ord a, FromBool a, Joinable b) => Interval a -> b -> b -> b
if_ (Interval l h) t f =
    let zero = fromBool False
    in  if
    | l > zero && h > zero   -> t
    | l < zero && h < zero   -> t
    | l == zero && h == zero -> f
    | otherwise              -> t `join` f
