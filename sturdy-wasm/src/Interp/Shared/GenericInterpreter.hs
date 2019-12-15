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
import Control.Arrow.Wasm

class (Arrow a) => IsVal v f a | a -> f, a -> v where
    const :: a WasmVal v
    binary :: a (WasmType, BinOpInstr, v, v) v
    unary :: a (WasmType, UnOpInstr, v) v
    compare :: a (WasmType, RelOpInstr, v, v) v
    br :: a Int ()
    onExit :: a () ()
    if_ :: a (a x z, a y z) (a (x, y) z)

-- interp :: (IsVal v f a, ArrowRun a, ArrowFail e a, ArrowWasm a,
--     ArrowFix [Instr] [v]) => a [Instr] [v]
-- interp = fix $ \interp' -> proc instrs -> case instrs of
--     [] ->
