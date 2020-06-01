module Interp.SharedHO.Data.Value
    ( Type(..)
    , Value(..)
    , getType
    ) where

import Data.Int

import Interp.SharedHO.Data.Joinable
import Interp.SharedHO.Data.BoolVal

data Type
    = I32
    | I64
    deriving (Show, Eq)

data Value = I32Val Int32 | I64Val Int64 deriving Eq

toInteger' :: Value -> Integer
toInteger' (I32Val n) = toInteger n
toInteger' (I64Val n) = toInteger n

getType :: Value -> Type
getType (I32Val _) = I32
getType (I64Val _) = I64

instance Show Value where
    show v = show (getType v) ++ ":" ++ show (toInteger' v)

instance Ord Value where
    v1 <= v2 = toInteger' v1 <= toInteger' v2

unValOp :: (Integer -> Integer) -> Value -> Value
unValOp op (I32Val n) = I32Val $ fromInteger $ op $ toInteger n
unValOp op (I64Val n) = I64Val $ fromInteger $ op $ toInteger n

binValOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
binValOp op (I64Val n1) (I64Val n2) = I64Val $ fromInteger
    $ (toInteger n1) `op` (toInteger n2)
binValOp op v1 v2 = fromInteger $ (toInteger' v1) `op` (toInteger' v2)

instance Num Value where
    (+)         = binValOp (+)
    (*)         = binValOp (*)
    negate      = unValOp negate
    abs         = unValOp abs
    signum      = unValOp signum
    fromInteger n = I32Val $ fromInteger n

instance ToBool Value where
    toBool v = 0 /= toInteger' v

instance FromBool Value where
    fromBool b = if b then fromInteger 1 else fromInteger 0
