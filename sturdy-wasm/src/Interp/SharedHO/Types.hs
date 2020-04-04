module Interp.SharedHO.Types 
where

data Type
    = I32
    | I64
    deriving (Show, Eq)

data Value = Value {
    getType :: Type,
    getVal :: Integer
} deriving (Show, Eq)