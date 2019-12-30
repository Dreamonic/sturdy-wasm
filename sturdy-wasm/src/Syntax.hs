module Syntax
    ( Instr(..)
    , BinOpInstr(..)
    , UnOpInstr(..)
    , RelOpInstr(..)
    , Param(..)
    , Signedness(..)
    , Func(..)
    , WasmModule(..)
    ) where

import Types

data Instr
    = Const WasmVal
    | Block [WasmType] [Instr]
    | Br Int
    | BrIf Int
    | If [WasmType] [Instr] [Instr]
    | Loop [WasmType] [Instr]
    | Call String
    | LocalGet String
    | LocalSet String
    | LocalTee String
    | Binary WasmType BinOpInstr
    | Unary WasmType UnOpInstr
    | Compare WasmType RelOpInstr
    | Nop
    | Unreachable
    deriving (Show, Eq)

data BinOpInstr
    = Add
    | And
    | Sub
    | Mul
    | Div Signedness
    | Or
    | Xor deriving (Show, Eq)

data UnOpInstr
    = Neg
    | Abs deriving (Show, Eq)

data RelOpInstr
    = Eql deriving (Show, Eq)

data Param = Param { getName :: String, getValue :: WasmType }
    deriving (Show, Eq)

data Signedness = Signed | Unsigned deriving (Show, Eq)

data Func = Func { fuName :: String, fuParams :: [Param], fuRty :: [WasmType],
                   fuInstrs :: [Instr] } deriving (Show, Eq)

data WasmModule = WasmModule { modFuncs :: [Func] } deriving (Show, Eq)
