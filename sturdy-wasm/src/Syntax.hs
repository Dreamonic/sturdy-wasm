module Syntax
    ( Instr(..)
    , BinOpInstr(..)
    , UnOpInstr(..)
    , RelOpInstr(..)
    , Param(..)
    , getName
    , getValue
    , Result(..)
    , getResult
    , Signedness(..)
    , Func(..)
    , fName
    , WasmModule(..)
    , modFuncs
    ) where

import Types

data Instr
    = Const WasmVal
    | Block [WasmType] [Instr]
    | Br Integer
    | BrIf Integer
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

data Result = Result { getResult :: WasmType } deriving (Show, Eq)

data Signedness = Signed | Unsigned deriving (Show, Eq)

data Func = Func { fName :: String, fParams :: [Param], fRes :: [Result],
                   fIs :: [Instr] } deriving (Show, Eq)

data WasmModule = WasmModule { modFuncs :: [Func] } deriving (Show, Eq)
