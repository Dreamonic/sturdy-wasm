module WasmTypes(
  WasmType(..)
  , WasmVal(..)
  , ofType
  , toWasmI
  , toWasmF
  , (<|>)
  , (<:*:>)
  , (<%>)
  , (<=>)
  , ret
  , wasmfmap
) where

import Prelude(Show, Ord, Eq, Integer, Double, Bool, error, fromIntegral, round, (==), ($))
import Data.Bool

data WasmType
    = I32
    | I64
    | F32
    | F64
    deriving (Show, Eq)  

data WasmVal
    = I32Val Integer
    | I64Val Integer
    | F32Val Double
    | F64Val Double
    deriving (Show, Eq)

infixl 0 <%>
infixl 1 :<*>

class WasmFunctor a where
    wasmfmap :: (a -> a) -> WasmVal -> WasmVal

class WasmApplicative a where
    (<%>) :: a -> Applicable WasmVal-> WasmVal

class Function a where
    ret :: (a -> a -> a) -> F

data Applicable a = a :<*> a
data FApplicable a b = (a -> a -> a) :<|> (b -> b -> b)
data CApplicable a b = (a -> a -> Bool) :<=> (b -> b -> Bool)
data F
    = FI (Integer -> Integer -> Integer)
    | FF (Double -> Double -> Double)
    | FP (FApplicable Integer Double)
    | FC (CApplicable Integer Double)

(<:*:>) :: WasmVal -> WasmVal -> Applicable WasmVal
x <:*:> y = x :<*> y

(<|>) :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> F
fi <|> ff = FP (fi :<|> ff)

(<=>) :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> F
fi <=> ff = FC (fi :<=> ff)

instance WasmFunctor Integer where
    wasmfmap f (I32Val x) = I32Val (f x)
    wasmfmap f (I64Val x) = I64Val (f x)

instance WasmApplicative F where
    FI f <%> (I32Val a) :<*> (I32Val b) = I32Val (f a b)
    FI f <%> (I64Val a) :<*> (I64Val b) = I64Val (f a b)
    FF f <%> (F32Val a) :<*> (F32Val b) = F32Val (f a b)
    FF f <%> (F64Val a) :<*> (F64Val b) = F64Val (f a b)
    FP (fi :<|> ff) <%> (I32Val a) :<*> (I32Val b) = I32Val (fi a b)
    FP (fi :<|> ff) <%> (I64Val a) :<*> (I64Val b) = I64Val (fi a b)
    FP (fi :<|> ff) <%> (F32Val a) :<*> (F32Val b) = F32Val (ff a b)
    FP (fi :<|> ff) <%> (F64Val a) :<*> (F64Val b) = F64Val (ff a b)
    FC (fi :<=> ff) <%> (I32Val a) :<*> (I32Val b) = boolToWasm $ fi a b
    FC (fi :<=> ff) <%> (I64Val a) :<*> (I64Val b) = boolToWasm $ fi a b
    FC (fi :<=> ff) <%> (F32Val a) :<*> (F32Val b) = boolToWasm $ ff a b
    FC (fi :<=> ff) <%> (F64Val a) :<*> (F64Val b) = boolToWasm $ ff a b
    f <%> _ = error ("Cannot cast between types.")

instance Function Integer where
    ret = FI

instance Function Double where
    ret = FF

getType :: WasmVal -> WasmType
getType typ = case typ of
    (I32Val _) -> I32
    (I64Val _) -> I64
    (F32Val _) -> F32
    (F64Val _) -> F64

boolToWasm :: Bool -> WasmVal
boolToWasm x = case x of
    True -> I32Val 1
    False -> I32Val 0

ofType :: WasmVal -> WasmType -> Bool
ofType val typ = (getType val) == typ

toWasmI :: WasmType -> Integer -> WasmVal
toWasmI typ x = case typ of
    I32 -> I32Val x
    I64 -> I64Val x
    _   -> toWasmF typ (fromIntegral x)

toWasmF :: WasmType -> Double -> WasmVal
toWasmF typ x = case typ of
    F32 -> F32Val x
    F64 -> F64Val x
    _   -> toWasmI typ (round x)