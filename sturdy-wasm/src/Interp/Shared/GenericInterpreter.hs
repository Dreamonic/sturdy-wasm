{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Interp.Shared.GenericInterpreter
(
) where

import Prelude hiding (compare, const)
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Lens hiding (Const)

import Syntax
import Types
import Control.Arrow.Wasm

class (Arrow c) => IsVal v fd c | c -> fd, c -> v where
    const :: c WasmVal v
    block :: c ([WasmType], [Instr]) (Frame v fd)
    loop :: c ([WasmType], [Instr]) (Frame v fd)
    binary :: c (WasmType, BinOpInstr, v, v) v
    unary :: c (WasmType, UnOpInstr, v) v
    compare :: c (WasmType, RelOpInstr, v, v) v
    br :: c Int [Instr]
    onExit :: c () ()
    if_ :: c x z -> c y z -> c (x, y) z

interp :: (IsVal v fd c, ArrowRun c, ArrowFail e c, ArrowWasm v fd c,
    ArrowFix (c [Instr] [v])) => c [Instr] [v]
interp = fix $ \interp' -> proc instrs -> case instrs of
    [] -> do
        onExit -< ()
        fr <- popF -< ()
        pushManyV -< view frVStack fr
        interp' -< view frIStack fr
    (Const v):is -> do
        pushV <<< const -< v
        interp' -< is
    (Block tys bIs):is -> do
        fr <- block -< (tys, bIs)
        pushF -< fr
        interp' -< bIs
    (Br n):is -> do
        is' <- br -< n
        interp' -< is'
    (BrIf n):is -> interp' -< (If [Br n] [] []):is
    (If ifIs elseIs tys) -> do
        fr
        if_ interp' interp' -< (ifIs, elseIs)
