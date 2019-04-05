module MonadicExecutor(
    MExecutor(..)
    , ModInst(..)
    , Locals(..)
    , Frame(..)
    , Stack(..)
    , Code(..)
    , Closure(..)
    , AdminInstr(..)
    , Config(..)
    , initConfig
    , unEnv
    , push
    , pop
    , setVar
    , getVar
    , putInstr
    , putInstrList
    , getInstr
    , hasInstr
) where

import qualified Data.Map as Map
import Parser
import Debug.Trace

data ModInst =
    EmptyInst
    deriving (Show, Eq)
  
type Locals = Map.Map String WasmVal

data Frame =
    FrameT ModInst Locals
    deriving (Show, Eq)

type Stack a = [a]

data Code =
    Code (Stack WasmVal) [AdminInstr]
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

data ExecutorException
    = TypeError
    | LookupError
    | NumericError
    | TrapError

-- type SafeExecutor a = ExceptT ExecutorException MExecutor a

newtype MExecutor a = Env (Config -> (a, Config))

instance Functor MExecutor where
    fmap f e = Env (\n ->   let (a, n1) = (unEnv e) n
                            in  (f a, n1))

instance Applicative MExecutor where
    pure = return
    f <*> v = Env (\n ->    let (a, n1) = (unEnv f) n
                                (b, n2) = (unEnv v) n1
                            in  (a b, n2))

instance Monad MExecutor where
    return x = Env (\n -> (x, n))
    e >>= f = Env (\n ->    let (a, n1) = (unEnv e) n
                                (b, n2) = unEnv (f a) n1
                            in  (b, n2))

-- |    Initialize the Config to Config with no locals, 
--      an empty value stack, and an empty execution stack.
initConfig :: Stack WasmVal -> AdminInstr -> Config
initConfig vs e = Config (FrameT EmptyInst Map.empty) (Code vs [e])

-- |    Unencapsulate the state function.
unEnv :: MExecutor a -> (Config -> (a, Config))
unEnv (Env e) = e

-- |    Push a WasmVal onto the value stack.
push :: WasmVal -> MExecutor ()
push v = Env (\(Config frame (Code vs es)) -> ((), Config frame (Code (v:vs) es)))

-- |    Pop a single WasmVal from the value stack.
pop :: MExecutor WasmVal
pop = Env (\(Config frame (Code (v:vs) es)) -> (v, Config frame (Code vs es)))

-- |    Put a value into the local environment
setVar :: String -> WasmVal -> MExecutor () 
setVar id v = Env (\(Config (FrameT m locals) c) -> ((), Config (FrameT m (Map.insert id v locals)) c))

-- |    Get a value from the local environment specified by the variable.
getVar :: String -> MExecutor WasmVal
getVar id = Env (\(Config (FrameT m locals) c) -> (findLocal id locals, Config (FrameT m locals) c))

-- |    Put an instruction on the execution stack.
putInstr :: AdminInstr -> MExecutor ()
putInstr instr = Env (\(Config frame (Code vs es)) -> ((), Config frame (Code vs (instr:es))))

-- |    Put a list of instructions on the execution stack.
putInstrList :: [AdminInstr] -> MExecutor ()
putInstrList is = Env (\(Config frame (Code vs es)) -> ((), Config frame (Code vs (is ++ es))))

-- |    Get the first instruction from the execution stack.
getInstr :: MExecutor AdminInstr
getInstr = Env (\(Config frame (Code vs (e:es))) -> (e, Config frame (Code vs es)))


hasInstr :: MExecutor Bool
hasInstr = Env (\(Config frame (Code vs es)) -> case es of
    [] -> (False, Config frame (Code vs es)) 
    _ -> (True, Config frame (Code vs es)))

-- |    Find a local variable from local environment.
findLocal :: String -> Locals -> WasmVal
findLocal id locals = case Map.lookup id locals of
  Just v -> v
  Nothing -> error "Value not found"