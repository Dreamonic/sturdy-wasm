{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Interp.Shared.GenericInterpreterAlt
(
) where

import Prelude hiding (compare, const)
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Fail
import Control.Arrow.Fix

import Syntax
import Types

class (Arrow c) => IsVal v c | c -> v where
    next :: c () Instr
    const :: c WasmVal v
    block :: c ([WasmType], [Instr]) ()
    loop :: c ([WasmType], [Instr]) ()
    binary :: c (WasmType, BinOpInstr, v, v) v
    unary :: c (WasmType, UnOpInstr, v) v
    compare :: c (WasmType, RelOpInstr, v, v) v
    br :: c Int ()
    onExit :: c () ()
    if_ :: c x z -> c y z -> c (x, y) z
