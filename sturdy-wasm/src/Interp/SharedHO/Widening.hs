module Interp.SharedHO.Widening
where

import qualified Data.Set as S

class StdB a where
    stdB :: S.Set a

class Widening a where
    widening :: a -> a -> a
