module Parser(
  WasmType(..)
  , WasmModule(..)
  , Func(..)
  , Instr(..)
  , TypedInstr(..)
  , WasmVal(..)
  , Block(..)
  , Param(..)
  , Result(..)
  , sameType
) where

data WasmType
  = I32
  | I64
  | F32
  | F64
  deriving (Show, Eq)

data Instr
  = EnterBlock Block
  | Branch Integer
  | If Instr
  | Loop Instr
  | Call String
  | LocalGet String
  | LocalSet String
  | LocalTee String
  | Numeric TypedInstr
  | Nop
  deriving (Show, Eq)

data TypedInstr
  = Const WasmVal
  | Add WasmType
  | Sub WasmType
  | Mul WasmType
  | Div WasmType SignedNess
  | Rem WasmType SignedNess
  | And WasmType
  | Or WasmType
  | Xor WasmType
  | Abs WasmType
  | Neg WasmType
  deriving (Show, Eq)

data WasmVal
  = I32Val Integer
  | I64Val Integer
  | F32Val Double
  | F64Val Double
  deriving (Show, Eq)

getType :: WasmVal -> WasmType
getType (I32Val _) = I32
getType (I64Val _) = I64
getType (F32Val _) = F32
getType (F64Val _) = F64

sameType :: WasmVal -> WasmType -> Bool
sameType val typ = (getType val) == typ

toWasmI :: WasmType -> Integer -> WasmVal
toWasmI I32 x = I32Val x
toWasmI I64 x = I64Val x
toWasmI typ x = toWasmF typ (fromIntegral x)

toWasmF :: WasmType -> Double -> WasmVal
toWasmF F32 x = F32Val x
toWasmF F64 x = F64Val x
toWasmF typ x = toWasmI typ (round x)

data Param = Param String WasmType deriving (Show, Eq)
data Result = Result WasmType deriving (Show, Eq)
data SignedNess = Signed | Unsigned deriving (Show, Eq)
data Block = Block [Result] [Instr] deriving (Show, Eq)
data Func = Func String [Param] Block deriving (Show, Eq)
data WasmModule = WasmModule [Func] deriving (Show, Eq)
