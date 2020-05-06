module Interp.SharedHO.Data.AbstractState
where

class AbstractState s where
    emptySt :: s

instance AbstractState () where
    emptySt = ()
