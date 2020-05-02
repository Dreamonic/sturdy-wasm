module Interp.SharedHO.Data.BoolVal
where

class ToBool a where
    toBool :: a -> Bool

class FromBool a where
    fromBool :: Bool -> a
