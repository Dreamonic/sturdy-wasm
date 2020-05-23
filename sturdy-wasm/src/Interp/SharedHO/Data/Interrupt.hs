module Interp.SharedHO.Data.Interrupt where

data Interrupt = Error String | Branching Int | Returning deriving (Eq, Show)
