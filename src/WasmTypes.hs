module WasmTypes(
  WasmType(..)
  , WasmVal(..)
  , ofType
  , toWasmI
  , toWasmF
) where

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

infixl 0 <$>
infixl 1 :<*>

class WasmFunctor a where
    fmap :: (a -> a) -> WasmVal -> WasmVal

class WasmApplicative a where
    (<$>) :: (a -> a -> a) -> Applicable WasmVal-> WasmVal

data Applicable a = a :<*> a

instance WasmFunctor Integer where
    fmap f (I32Val x) = I32Val (f x)
    fmap f (I64Val x) = I64Val (f x)


instance WasmApplicative Integer where
    f <$> (I32Val a) :<*> (I32Val b) = I32Val (f a b)
    f <$> (I64Val a) :<*> (I64Val b) = I64Val (f a b)
    f <$> _ = error ("Cannot cast between types.")

instance WasmApplicative Double where
    f <$> (F32Val a) :<*> (F32Val b) = F32Val (f a b)
    f <$> (F64Val a) :<*> (F64Val b) = F64Val (f a b)
    f <$> _ = error ("Cannot cast between types.")

getType :: WasmVal -> WasmType
getType typ = case typ of
    (I32Val _) -> I32
    (I64Val _) -> I64
    (F32Val _) -> F32
    (F64Val _) -> F64
  

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