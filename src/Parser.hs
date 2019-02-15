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
  , SignedNess(..)
  , ofType
  , parse
  , function
) where

import           Control.Applicative
import           Control.Monad

import           Lexer

import Data.List


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
  | Div WasmType SignedNess   -- TODO: keep it like this, or introduce seperate Div instructions (one for floats, and signed and unsigned)
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
getType typ = case typ of
  (I32Val _) -> I32
  (I64Val _) -> I64
  (F32Val _) -> F32
  (F64Val _) -> F64

ofType :: WasmVal -> WasmType -> Bool
ofType val typ = (getType val) == typ

toWasmI :: WasmType -> Integer -> WasmVal
toWasmI typ x = case typ of
  I32 -> I32Val x
  I64 -> I64Val x
  _   -> toWasmF typ (fromIntegral x)

toWasmF :: WasmType -> Double -> WasmVal
toWasmF typ x = case typ of
  F32 -> F32Val x
  F64 -> F64Val x
  _   -> toWasmI typ (round x)

data Param = Param String WasmType deriving (Show, Eq)
data Result = Result WasmType deriving (Show, Eq)
data SignedNess = Signed | Unsigned deriving (Show, Eq)
data Block = Block [Result] [Instr] deriving (Show, Eq)
data Func = Func String [Param] Block deriving (Show, Eq)
data WasmModule = WasmModule [Func] deriving (Show, Eq)


{-
  Parser based on the paper Monadic Parsing in Haskell

  maybe we should use e.g. parsec instead of defining these funtions
-}
newtype Parser a = Parser ([Token] -> [(a, [Token])])

parse (Parser p) = p

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  p >>= f = Parser (\cs -> concat [parse (f a) cs' |
    (a,cs') <- parse p cs])

instance MonadPlus Parser where
  mzero = Parser (\cs -> [])
  mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

instance Alternative Parser where
  empty  = mzero
  p <|> q = Parser(\s -> 
    case parse p s of
      []  -> parse q s
      res -> res)

item :: Parser Token
item = Parser (\cs -> case cs of
  []     -> []
  (c:cs) -> [(c,cs)])

sat  :: (Token -> Bool) -> Parser Token
sat p = do 
  c <- item
  if p c 
    then return c 
    else mzero

{-
    End parsing primitives
-}


parens :: Parser a -> Parser a
parens p = do
  sat $ isToken LP
  res <- p
  sat $ isToken RP
  return res

{- Functions that extract values from a token -}
extractIDString :: Token -> Parser String
extractIDString (ID str) = return str
extractIDString _        = mzero

extractReserved :: Token -> Parser String
extractReserved (Reserved str) = return str
extractReserved _              = mzero

extractKeyword :: Token -> Parser String
extractKeyword (Keyword str) = return str
extractKeyword _             = mzero

extractFloatN :: Token -> Parser Double
extractFloatN (FloatN number) = return number
extractFloatN _               = mzero

extractInt :: Token -> Parser Integer
extractInt (SignedN number)   = return number
extractInt (UnsignedN number) = return number
extractInt _                  = mzero

extractTypedInstruction :: Token -> Parser (WasmType, String)
extractTypedInstruction (Keyword str)
  | Just func <- stripPrefix "i32." str = return (I32, func)
  | Just func <- stripPrefix "i64." str = return (I64, func)
  | Just func <- stripPrefix "f32." str = return (F32, func)
  | Just func <- stripPrefix "f64." str = return (F64, func)
extractTypedInstruction _               = mzero

isInt :: WasmType -> Bool
isInt I32 = True
isInt I64 = True
isInt _   = False

isFloat :: WasmType -> Bool
isFloat F32 = True
isFloat F64 = True
isFloat _   = False

float :: Parser Double
float = do
  tkn <- sat isFloatN
  extractFloatN tkn

int :: Parser Integer
int = do
  tkn <- sat isIntN
  extractInt tkn
-- wrapWasmVal :: Num a -> WasmVal a

parseInstruction = parseGetLocal <|> parseNumericInstr

parseGetLocal :: Parser Instr
parseGetLocal = do
  keyword "get_local"
  return LocalGet <*> idStr

parseConst :: Parser Instr
parseConst = do
  keyword <- sat isKeyword
  (t, i) <- extractTypedInstruction keyword
  if i /= "const"
    then
      mzero
    else
      case t of
        F32 -> Numeric . Parser.Const . F32Val <$> float
        F64 -> Numeric . Parser.Const . F64Val <$> float
        I32 -> Numeric . Parser.Const . I32Val <$> int
        I64 -> Numeric . Parser.Const . I64Val <$> int

parseNumericInstr :: Parser Instr
parseNumericInstr = parseConst <|> do
  keyword <- sat isKeyword
  (t, i) <- extractTypedInstruction keyword
  typedInstr <- case t of
    t | isInt t   -> makeII i t
    t | isFloat t -> makeFI i t
  return (Numeric typedInstr)

-- makeIntegerInstruction
makeII :: String -> WasmType -> Parser TypedInstr
makeII "add" t   = return $ Add t
makeII "sub" t   = return $ Sub t
makeII "mul" t   = return $ Mul t
makeII "div_s" t = return $ Div t Signed
makeII "div_u" t = return $ Div t Unsigned
makeII "rem_s" t = return $ Rem t Signed
makeII "rem_u" t = return $ Rem t Unsigned
makeII "and" t   = return $ And t
makeII "or"  t   = return $ Or t
makeII "xor" t   = return $ Xor t

-- makeFloatInstruction
makeFI :: String -> WasmType -> Parser TypedInstr
makeFI "add" t = return $ Add t
makeFI "sub" t = return $ Sub t
makeFI "mul" t = return $ Mul t
makeFI "div" t = return $ Div t Signed
makeFI "abs" t = return $ Abs t
makeFI "neg" t = return $ Neg t

isToken :: Token -> Token -> Bool
isToken tkn = (== tkn)

isKeyword :: Token -> Bool
isKeyword (Keyword _) = True
isKeyword _           = False

isId :: Token -> Bool
isId (ID _) = True
isId _      = False

isFloatN :: Token -> Bool
isFloatN (FloatN _) = True
isFloatN _          = False

isIntN :: Token -> Bool
isIntN (SignedN _)   = True
isIntN (UnsignedN _) = True
isIntN _             = False

-- succeeds if the next token is a keyword with string str, fails otherwise
keyword :: String -> Parser Token
keyword str = sat $ isToken (Keyword str)

idStr :: Parser String
idStr = do
  id <- sat isId
  extractIDString id

parseType :: Parser WasmType
parseType = do
  tkn <- item
  typStr <- extractKeyword tkn
  case typStr of
    "i32" -> return I32
    "i64" -> return I64
    "f32" -> return F32
    "f64" -> return F64
    _     -> mzero

param :: Parser Param
param = parens $ do
  sat $ isToken (Keyword "param")
  idstr <- idStr
  typ   <- parseType
  return (Param idstr typ)

parseResultType :: Parser Result
parseResultType = parens $ do
  keyword "result"
  typ   <- parseType
  return (Result typ)


function :: Parser Func
function = parens $ do
  keyword "func"
  idstr <- idStr
  params <- many param
  resultTypes <- many parseResultType
  instr  <- many parseInstruction
  return $ Func idstr params $ Block resultTypes instr


-- TODO: currently only handles function definitions, not exports etc
parseModule :: Parser WasmModule
parseModule = parens $ do
  keyword "module"
  functions <- many function
  return (WasmModule functions)

runParser p tkns = case parse p tkns of
  [(res, [])]      -> res
  [(res, tkns)]    -> error "Not all tokens were consumed"
  _              -> error "Parse error"

-- parseKeyword = parseInstruction

wattokens = [LP,Keyword "func",ID "add",LP,Keyword "param",ID "lhs",Keyword "i32",RP,LP,Keyword "param",ID "rhs",Keyword "i32",RP,LP,Keyword "result",Keyword "i32",RP,Keyword "get_local",ID "lhs",Keyword "get_local",ID "rhs",Keyword "i32.add",RP]

watfunc = "(func $add (param $lhs i32) (param $rhs i32) (result i32)get_local $lhs get_local $rhs i32.add)"