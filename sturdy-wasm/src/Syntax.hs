module Syntax where

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
    | Div SignedNess
    | Or
    | Xor deriving (Show, Eq)

data UnOpInstr
    = Neg
    | Abs deriving (Show, Eq)

data RelOpInstr
    = Eql deriving (Show, Eq)

data Param = Param {getName :: String, getValue :: WasmType} deriving (Show, Eq)
data Result = Result WasmType deriving (Show, Eq)
data SignedNess = Signed | Unsigned deriving (Show, Eq)
data Func = Func String [Param] [Result] [Instr] deriving (Show, Eq)
data WasmModule = WasmModule [Func] deriving (Show, Eq)

getResult :: Result -> WasmType
getResult (Result t) = t

isInt :: WasmType -> Bool
isInt I32 = True
isInt I64 = True
isInt _   = False

isFloat :: WasmType -> Bool
isFloat F32 = True
isFloat F64 = True
isFloat _   = False