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

setStack :: Stack -> Environment -> Environment
setStack s (Environment _ l g) = Environment s l g

stackHead :: Environment -> WasmVal
stackHead env = head (stack env)

stackTail :: Environment -> [WasmVal]
stackTail env = tail (stack env)

stackPush :: WasmVal -> Environment -> Environment
stackPush x env = setStack (x:(stack env)) env

stackPopN :: Int -> Environment -> Environment
stackPopN n env = setStack (drop n (stack env)) env

stackPop :: Environment -> Environment
stackPop = stackPopN 1

fromStack :: Stack -> Environment
fromStack s = setStack s emptyEnv

setLoc :: WasmValMap -> Environment -> Environment
setLoc l (Environment s _ g) = Environment s l g

clearLoc :: Environment -> Environment
clearLoc = setLoc M.empty

insertLoc :: String -> WasmVal -> Environment -> Environment
insertLoc tag x env = setLoc (M.insert tag x (loc env)) env

setGlob :: Environment -> WasmValMap -> Environment
setGlob (Environment s l _) g = Environment s l g
