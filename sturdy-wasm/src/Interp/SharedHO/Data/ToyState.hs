{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.Data.ToyState
where

import qualified Data.Map as M
import Control.Lens

import Interp.SharedHO.Data.Joinable
import Interp.SharedHO.Data.Widening

data ToyState v = ToyState { _variables :: M.Map String v
                           , _stack :: [v] } deriving (Show, Eq)

emptyToySt = ToyState M.empty []

makeLenses ''ToyState

instance Joinable a => Joinable (ToyState a) where
    join st1 st2 = over stack (join $ view stack st2) $
                   over variables (join $ view variables st2) st1

instance (Widening a, Show a) => Widening (ToyState a) where
    widening st1 st2 = ToyState (_variables st1 `widening` _variables st2)
                                (_stack st1 `widening` _stack st2)
