module Parser(
  WasmType(..)
  , Param(..)
  , Result(..)
  , Instr(..)
  , TypedInstr(..)
  , Func(..)
  , Block(..)
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

sameType :: WasmVal -> WasmType -> Bool
sameType (I32Val _) I32 = True
sameType (I64Val _) I64 = True
sameType (F32Val _) F32 = True
sameType (F64Val _) F64 = True
sameType _ _            = False

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
extractKeyword _              = mzero


parseInstruction = parseGetLocal <|> parseTypedInstruction

parseGetLocal :: Parser Instr
parseGetLocal = do
  keyword "get_local"
  return LocalGet <*> idStr

{- 
  parsing typed instructions:
  note: not sure if it is actually a good idea to try to generalize this,
    not all combinations of types and numeric instructions actually exist
-}
parseTypedInstruction :: Parser Instr
parseTypedInstruction = do
  keyword <- sat isKeyword
  (t, i) <- extractTypedInstruction keyword
  typedInstr   <- makeTI i t
  return (Numeric typedInstr)

extractTypedInstruction :: Token -> Parser (WasmType, String)
extractTypedInstruction (Keyword str)
  | Just func <- stripPrefix "i32." str = return (I32, func)
  | Just func <- stripPrefix "i64." str = return (I64, func)
  | Just func <- stripPrefix "f32." str = return (F32, func)
  | Just func <- stripPrefix "f64." str = return (F64, func)
extractTypedInstruction _               = mzero

-- TODO: div only exists for floats, ints have div_s and div_u
makeTI :: String -> WasmType -> Parser TypedInstr
makeTI "add" t = return $ Add t
makeTI "sub" t = return $ Add t
makeTI "mul" t = return $ Add t
makeTI "div" t = return $ Add t


isToken :: Token -> Token -> Bool
isToken tkn = (== tkn)

isKeyword :: Token -> Bool
isKeyword (Keyword _) = True
isKeyword _           = False

isId :: Token -> Bool
isId (ID _) = True
isId _      = False

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