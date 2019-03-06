module Environment
  ( Stack
  , WasmValMap
  , Environment(..)
  , emptyEnv
  , setStack
  , clearStack
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

-- |Defines an execution environment for functions in Executor.hs.
data Environment = Environment
    { stack :: Stack            -- ^The stack containing WasmVals.
    , loc :: WasmValMap         -- ^A map of all local variables in scope.
    , glob :: WasmValMap        -- ^A map of all global variables.
    } deriving (Show, Eq)

-- |An empty environment
emptyEnv :: Environment
emptyEnv = Environment [] M.empty M.empty

-- |Substitute the given Stack into the Environment.
setStack :: Stack -> Environment -> Environment
setStack s (Environment _ l g) = Environment s l g

-- |Replace the Stack of the Environment with an empty list.
clearStack :: Environment -> Environment
clearStack = setStack []

-- |Return the head of the Stack of the Environment.
stackHead :: Environment -> WasmVal
stackHead env = head (stack env)

-- |Return the tail of the Stack of the Environment.
stackTail :: Environment -> [WasmVal]
stackTail env = tail (stack env)

-- |Put the given WasmVal in on top of the Stack of the Environment.
stackPush :: WasmVal -> Environment -> Environment
stackPush x env = setStack (x:(stack env)) env

-- |Remove n elements from the top of the Stack of the Environment.
stackPopN :: Int -> Environment -> Environment
stackPopN n env = setStack (drop n (stack env)) env

-- |Remove one element from the top of the Stack of the Environment.
stackPop :: Environment -> Environment
stackPop = stackPopN 1

-- |Create an Environment with the given Stack. All other fields are initialized
--  as defined in emptyEnv.
fromStack :: Stack -> Environment
fromStack s = setStack s emptyEnv

-- |Substitute the given WasmValMap as local variables into the Environment.
setLoc :: WasmValMap -> Environment -> Environment
setLoc l (Environment s _ g) = Environment s l g

-- |Replace the local variables in the Environment with an empty Map.
clearLoc :: Environment -> Environment
clearLoc = setLoc M.empty

-- |Bind a WasmVal to a tag in the Environment.
insertLoc :: String -> WasmVal -> Environment -> Environment
insertLoc tag x env = setLoc (M.insert tag x (loc env)) env

-- |Substitute the given WasmValMap as global variables into the Environment.
setGlob :: Environment -> WasmValMap -> Environment
setGlob (Environment s l _) g = Environment s l g
