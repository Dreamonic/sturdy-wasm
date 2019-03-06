module Environment
  ( Stack
  , WasmValMap
  , Environment(..)
  , emptyEnv
  , setStack
  , stackHead
  , stackTail
  , stackPush
  , stackPopN
  , stackPop
  , fromStack
  , setLoc
  , clearLoc
  , insertLoc
  , setGlob
  ) where

import Parser
import qualified Data.Map as M

type Stack = [WasmVal]
type WasmValMap = M.Map String WasmVal

data Environment = Environment { stack :: Stack
                             , loc :: WasmValMap
                             , glob :: WasmValMap
                             } deriving (Show, Eq)

emptyEnv :: Environment
emptyEnv = Environment [] M.empty M.empty

setStack :: Environment -> Stack -> Environment
setStack (Environment _ l g) s = Environment s l g

stackHead :: Environment -> WasmVal
stackHead env = head (stack env)

stackTail :: Environment -> [WasmVal]
stackTail env = tail (stack env)

stackPush :: Environment -> WasmVal -> Environment
stackPush env x = setStack env (x:(stack env))

stackPopN :: Int -> Environment -> Environment
stackPopN n env = setStack env (drop n (stack env))

stackPop :: Environment -> Environment
stackPop = stackPopN 1

fromStack :: Stack -> Environment
fromStack s = setStack emptyEnv s

setLoc :: Environment -> WasmValMap -> Environment
setLoc (Environment s _ g) l = Environment s l g

clearLoc :: Environment -> Environment
clearLoc (Environment s l g) = Environment s M.empty g

insertLoc :: Environment -> String -> WasmVal -> Environment
insertLoc (Environment s l g) tag x = Environment s (M.insert tag x l) g

setGlob :: Environment -> WasmValMap -> Environment
setGlob (Environment s l _) g = Environment s l g
