module Interp.SharedHO.Data.Types
where

import Interp.SharedHO.Data.Joinable
import Interp.SharedHO.Data.BoolVal

data Type
    = I32
    | I64
    deriving (Show, Eq)

data Value = Value {
    getType :: Type,
    getVal :: Integer
}

instance Show Value where
    show (Value t v) = show t ++ ":" ++ show v

instance ToBool Value where
    toBool = ((/=) 0) . getVal

instance FromBool Value where
    fromBool b = if b then Value I32 1 else Value I32 0

unValOp :: (Integer -> Integer) -> Value -> Value
unValOp op v = Value (getType v) $ op $ getVal v

binValOpGeneric :: (Integer -> Integer -> a) -> Value -> Value -> a
binValOpGeneric op v1 v2 = getVal v1 `op` getVal v2

binValOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
binValOp op v1 v2 = Value (getType v1) $ binValOpGeneric op v1 v2

instance Eq Value where
    (==) = binValOpGeneric (==)

instance Ord Value where
    (<=) = binValOpGeneric (<=)

instance Num Value where
    (+)           = binValOp (+)
    (*)           = binValOp (*)
    negate        = unValOp negate
    abs           = unValOp abs
    signum        = unValOp signum
    fromInteger n = Value I32 n

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