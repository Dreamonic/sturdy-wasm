module Interp.SharedHO.Types 
where

import Interp.SharedHO.Joinable

data Type
    = I32
    | I64
    deriving (Show, Eq)

data Value = Value {
    getType :: Type,
    getVal :: Integer
} deriving (Show, Eq)

data MaybeType 
    = Known Type
    | Unknown
    deriving (Show)

instance Eq MaybeType where
    a == b = case (a, b) of
        (Known a', Known b') -> a' == b'
        _ -> True

instance Joinable MaybeType where
    join a b = case (a, b) of
        (Known a', Known b') | a' == b' -> a
        _ -> Unknown