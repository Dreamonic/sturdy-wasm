module Executor
  ( execRed
  , execFunc
  , eval
  , step
  , Config(..)
  , ModInst(..)
  , Locals(..)
  , Frame(..)
  , Code(..)
  , AdminInstr(..)
  , Closure(..)
  ) where

import Parser
import WasmTypes
import Debug.Trace
import qualified Data.Map as Map
import qualified Control.Exception as E

import Data.Int

{- Types -}

data ModInst =
  EmptyInst
  deriving (Show, Eq)

type Locals = Map.Map String WasmVal

data Frame =
  FrameT ModInst Locals
  deriving (Show, Eq)

type Stack a = [a]

data Code =
  Code { codeStack :: (Stack WasmVal), codeEs :: [AdminInstr]}
  deriving (Show, Eq)

data Closure =
  Closure ModInst Func
  deriving (Show, Eq)

data AdminInstr =
    Plain Instr
  | Invoke Closure
  | Trapping String {- Trap with error message -}
  | Returning (Stack WasmVal)
  | Breaking Integer (Stack WasmVal)
  | Label Int [Instr] Code
  | Frame Frame Code
  deriving (Show, Eq)

data Config =
  Config { confFrame :: Frame, confCode :: Code }
  deriving (Show, Eq)

{- reduction -}

step :: Config -> Config
step c@(Config frame (Code vs es)) = do
  let (frame', vs', es') = step' (head es) vs c
  -- traceShow (frame', vs', es') $
  Config frame' (Code vs' (es' ++ tail es))

-- |Intermediate step function
--  takes admin_instr and a value stack
--  returns the modified value stack
step' :: AdminInstr -> Stack WasmVal -> Config -> (Frame, Stack WasmVal, [AdminInstr])

-- Definitions for plain instructions
step' (Plain e) vs (Config frame _) = case (e, vs) of
  (Nop, vs) ->
    (frame, vs, [])

  (Unreachable, vs) ->
    (frame, vs, [Trapping "unreachable code executed"])

  (Const v, vs)
    -> (frame, v : vs, [])

  {- TODO: factor Numeric (binary, unary etc. out into its own function/file-}
  (Binary typ e', v2 : v1 : vs') ->
    case e' of
      (Add) -> (frame, (((+) <|> (+)) <%> (v1 <:*:> v2)) : vs', [])
      (Sub) -> (frame, (((-) <|> (-)) <%> (v1 <:*:> v2)) : vs', [])
      (Mul) -> (frame, (((*) <|> (*)) <%> (v1 <:*:> v2)) : vs', [])
      (Div Signed) -> (frame, ((div <|> (/)) <%> (v1 <:*:> v2)) : vs', [])
      err     -> error ("unimplemented numeric: " ++ show err)

  (Compare typ e', v2 : v1 : vs') ->
    case e' of
      (Eql) -> (frame, (((==) <=> (==)) <%> (v1 <:*:> v2)) : vs', [])
      err     -> error ("unimplemented compare: " ++ show err)

  ((Block resultTypes instructions), vs) -> do
    let inputLen = length resultTypes
    let code = Code [] (map Plain instructions)   -- ^map instructions in block to admin_instr
    (frame, vs, [Label inputLen [] code])

  ((Loop _ instructions), vs) -> do
    let code = Code [] (map Plain instructions)
    (frame, vs, [Label 0 [e] code])                      -- ^add loop instr e inside inner label

  ((If resultTypes trueBl elseBl), (I32Val 0):vs') ->
    (frame, vs', [Plain (Block resultTypes elseBl)])

  ((If resultTypes trueBl elseBl), (I32Val _):vs') ->
    (frame, vs', [Plain (Block resultTypes trueBl)])

  ((Br x), vs) ->
    (frame, [], [Breaking x vs])

  ((BrIf _), (I32Val 0):vs') ->
    (frame, vs', [])

  ((BrIf x), (I32Val _):vs') ->
    (frame, vs', [Plain (Br x)])

  (LocalGet e', vs) ->
    (frame, (findLocal e' frame):vs, [])

  (LocalSet e', vs) ->
    case vs of
      v : vs' -> (addBind frame e' v, vs', [])

  err -> error $ "unimplemented plain: " ++ show err

-- End of label
-- should continue evalution
step' (Label _ _ (Code vs' [])) vs (Config frame _) =
  (frame, vs' ++ vs, [])

-- Trap inside label
-- current stack and msg is returned
step' (Label _ _ (Code _ ((Trapping msg):_))) vs (Config frame _) =
  (frame, vs, [Trapping msg])

-- Return inside label
-- current stack and return stack are returned
step' (Label _ _ (Code _ ((Returning vs'):_))) vs (Config frame _) =
  (frame, vs, [Returning vs'])

-- 'Break' to end of this label
-- the required amount of values are popped from the inner stack
-- and pushed on the outer stack
-- if the label has inside instructions (Loop does this)
-- the inside instructions are returned
step' (Label n inner (Code _ ((Breaking 0 retStack):_))) vs (Config frame _) =
  (frame, (take (fromIntegral n :: Int) retStack) ++ vs, map Plain inner)

-- 'Break' k label layers up
-- returns new Breaking instr with k-1
step' (Label n instr (Code vs' ((Breaking k retStack):es))) vs (Config frame _) =
  (frame, vs, [Breaking (k - 1) retStack])

-- Continue evaluation of instructions inside label
-- step code inside label in context of current config
-- return Label with resulting code
step' (Label n innerInstr code') vs c@(Config frame _) =
  let (Config frame' (Code vs' c')) = step $ c { confCode = code' }        -- ^Step under c with the code inside label
  in (frame', vs, [Label n innerInstr (Code vs' c')])

step' (Invoke (Closure _ (Func name params _ instr))) vs (Config frame _) =
  let (frame', vs') = addBinds frame params vs
  in (frame, (eval (Config frame' (Code [] (fmap Plain instr)))) ++ vs', [])

step' err _ _ = error $"Not implemented" ++ show err

-- |Evaluates code under given context
--  based on proposition 4.2 evalution should either
--    * return value stack
--    * trap
--    * modify context
eval :: Config -> Stack WasmVal
eval c@(Config _ (Code vs es)) = case es of
  [] -> vs
  (Trapping msg):_ -> error msg -- ^ TODO: change to other type of error handling
  _ -> eval $ step c

execFunc :: [WasmVal] -> Func -> Stack WasmVal
execFunc vs f = eval (Config (FrameT EmptyInst Map.empty)
    (Code vs [Invoke (Closure EmptyInst f)]))

execRed :: Func -> Config
execRed (Func name params results es) =
  stackToConfig $ eval (Config (FrameT EmptyInst Map.empty)
    (Code [] [Invoke (Closure EmptyInst (Func name params results es))]))

stackToConfig :: Stack WasmVal -> Config
stackToConfig vs = Config (FrameT EmptyInst Map.empty) (Code vs [])

addBinds :: Frame -> [Param] -> Stack WasmVal -> (Frame, Stack WasmVal)
addBinds (FrameT inst locals) params vs = case (params, vs) of
  ([], vs') -> ((FrameT inst locals), vs')
  (Param name _ : ps, v:vs') -> addBinds (FrameT inst (Map.insert name v locals)) ps vs'
  _ -> error "Expected more arguments"

addBind :: Frame -> String -> WasmVal -> Frame
addBind (FrameT inst locals) string value = FrameT inst (Map.insert string value locals)

findLocal :: String -> Frame -> WasmVal
findLocal id (FrameT _ locals) = case Map.lookup id locals of
  Just v -> v
  Nothing -> error "Value not found"
