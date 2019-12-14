{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Interp.Shared.GenericInterpreter
(
) where

import Control.Arrow
import Prelude hiding (compare)

import Syntax
import Types

data Bl = Bl {blInstrs :: [Instr], blRty :: [WasmType]}

class (Arrow a) => IsVal v f a | a -> f, a -> v where
    const :: a WasmVal v
    binary :: a (WasmType, BinOpInstr, v, v) v
    unary :: a (WasmType, UnOpInstr, v) v
    compare :: a (WasmType, RelOpInstr, v, v) v
    br :: a Int ()
    onExit :: a () ()
    if_ :: a (a Bl [v], a Bl [v]) (a (Bl, Bl) [v])
