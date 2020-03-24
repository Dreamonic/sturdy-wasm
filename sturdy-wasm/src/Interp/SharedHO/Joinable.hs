module Interp.SharedHO.Joinable
where

class Joinable a where
    join :: a -> a -> a
