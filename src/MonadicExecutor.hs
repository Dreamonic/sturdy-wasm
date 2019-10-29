{-# LANGUAGE TemplateHaskell #-}
module MonadicExecutor(
    MExecutor(..)
    , ModInst(..)
    , Locals(..)
    , FuncMap(..)
    , Frame(..)
    , Stack(..)
    , Code(..)
    , Closure(..)
    , AdminInstr(..)
    , Config(..)
    , label
    , code
    , buildConfig
    , setupFuncCall
    , unEnv
    , push
    , pop
    , setVar
    , getVar
    , putInstr
    , putInstrList
    , getInstr
    , hasInstr
    , retrieveStack
    , getStack
    , throwError
    , lookupFunc
) where

import qualified Data.Map as Map
import Parser
import Debug.Trace
import Control.Lens

data ModInst =
    EmptyInst
    deriving (Show, Eq)

type Locals = Map.Map String WasmVal
type FuncMap = Map.Map String Func

data Frame =
    FrameT {_modInst :: ModInst, _locals :: Locals, _funcs :: FuncMap}
    deriving (Show, Eq)

type Stack a = [a]

data Code =
    Code {_stack :: [WasmVal], _instr :: [AdminInstr]}
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
    Config { _confFrame :: Frame, _confCode :: Code }
    deriving (Show, Eq)

data ExecutorException
    = TypeError
    | LookupError
    | NumericError
    | TrapError


makeLenses ''Frame
makeLenses ''Code
makeLenses ''Config

label :: Int -> Code -> AdminInstr
label n c = Label n [] c

-- code :: [Instr] -> Code
-- code es = Code [] (fmap Plain es)

newtype MExecutor a = Env (Config -> (Either String a, Config))

instance Functor MExecutor where
    fmap f e = Env (\n ->   case (unEnv e) n of
                                (Right a1, n1) -> (Right (f a1), n1)
                                (Left msg, n1) -> (Left msg, n1))

instance Applicative MExecutor where
    pure = return
    f <*> v = Env (\n -> let (val1, n1) = (unEnv f) n
                             (val2, n2) = (unEnv v) n1
                         in  case (val1, val2) of
                             (Left msg1, Left msg2) -> (Left (msg1 ++ msg2), n2)
                             (Left msg1, _) -> (Left msg1, n2)
                             (_, Left msg2) -> (Left msg2, n2)
                             (Right a1, Right b1) -> (Right (a1 b1), n2))

instance Monad MExecutor where
    return x = Env (\n -> (Right x, n))
    e >>= f = Env (\n ->    case (unEnv e) n of
                                (Left msg, n1) -> (Left msg, n1)
                                (Right a1, n1) -> unEnv (f a1) n1)

-- |    Unencapsulate the state function.
unEnv :: MExecutor a -> (Config -> (Either String a, Config))
unEnv (Env e) = e

code :: [Instr] -> Code
code es = Code [] (fmap Plain es)

-- |    Construct a Config using the given WasmModule with
buildConfig :: WasmModule -> Config
buildConfig m = Config (FrameT EmptyInst Map.empty (funcMapFromModule m)) (Code [] [])

-- |    Retrieve a Map from a WasmModule from which Funcs can be fetched by
--      using the String of their name as a key.
funcMapFromModule :: WasmModule -> FuncMap
funcMapFromModule m = let funcs = funcsFromModule m
                      in  Map.fromList (zip (map funcName funcs) funcs)

-- |    Retrieve the Funcs stored in a WasmModule.
funcsFromModule :: WasmModule -> [Func]
funcsFromModule (WasmModule funcs) = funcs

-- |    Get the name of a Func as a String.
funcName :: Func -> String
funcName (Func name _ _ _) = name

-- |    Prepare the given Config for execution by inserting an instruction to
--      call the desired instruction and by filling the stack with the given
--      function arguments.
setupFuncCall :: String -> Stack WasmVal -> Config -> Config
setupFuncCall tag vs conf = set (confCode) (Code vs [Plain (Call tag)]) conf

-- |    Get the stack from a Config.
getStack :: Config -> Stack WasmVal
getStack = view (confCode . stack)

-- |    Set the stack of a Config.
setStack :: Stack WasmVal -> Config -> Config
setStack vs conf = set (confCode . stack) vs conf

-- |    Push a single WasmVal to the top of the stack of a Config.
pushToStack :: WasmVal -> Config-> Config
pushToStack v = over (confCode . stack) ((:) v)

-- |    Pop the top most value from the stack of a Config.
popFromStack :: Config -> (Either String WasmVal, Config)
popFromStack conf = case getStack conf of
    [] -> (Left "Cannot pop an empty stack", conf)
    v:vs -> (Right v, setStack vs conf)

popFromInstr :: Config -> (Either String AdminInstr, Config)
popFromInstr conf = case view (confCode . instr) conf of
    [] -> (Left "Cannot pop an empty instruction stack", conf)
    v:vs -> (Right v, set (confCode . instr) vs conf)

-- |    Push a WasmVal onto the value stack.
push :: WasmVal -> MExecutor ()
push v = Env (\config -> (Right (), (pushToStack v config)))

-- |    Pop a single WasmVal from the value stack.
pop :: MExecutor WasmVal
pop = Env (\config -> popFromStack config)

-- |    Put a value into the local environment
setVar :: String -> WasmVal -> MExecutor ()
setVar id v = Env (\config -> (Right (), over (confFrame . locals) (Map.insert id v) config))

-- |    Get a value from the local environment specified by the variable.
getVar :: String -> MExecutor WasmVal
getVar id = Env (\config -> (findLocal id (view (confFrame . locals) config), config))

-- |    Put an instruction on the execution stack.
putInstr :: AdminInstr -> MExecutor ()
putInstr i = Env (\config -> (Right (), over (confCode . instr) ((:) i) config))

-- |    Put a list of instructions on the execution stack.
putInstrList :: [AdminInstr] -> MExecutor ()
putInstrList is = Env (\config -> (Right (), over (confCode . instr) ((++) is) config))

-- |    Get the first instruction from the execution stack.
getInstr :: MExecutor AdminInstr
getInstr = Env (\config -> popFromInstr config)

hasInstr :: MExecutor Bool
hasInstr = Env (\config -> case view (confCode . instr) config of
    [] -> (Right False, config)
    _ ->  (Right True, config))

-- |    Get the full value stack from the Config.
retrieveStack :: MExecutor (Stack WasmVal)
retrieveStack = Env(\config -> (Right (view (confCode . stack) config), config))

-- |    Lookup the func with the specified tag from the functions loaded in
--      the Config.
lookupFunc :: String -> MExecutor Func
lookupFunc tag = Env (\config -> let f = findFunc tag (view (confFrame . funcs) config)
                                 in  (f, config))

-- |    Find a local variable from local environment.
findLocal :: String -> Locals -> Either String WasmVal
findLocal id locals = case Map.lookup id locals of
    Just v -> Right v
    Nothing -> Left ("The id: '" ++ id ++ "' was not bound")


throwError :: String -> MExecutor ()
throwError err = Env (\config -> (Left err, config))

-- |    Find a Func from a given FuncMap. Throw a run-time error if not found.
findFunc :: String -> FuncMap -> Either String Func
findFunc tag funcMap = case Map.lookup tag funcMap of
    Just f -> Right f
    Nothing -> Left ("Called function '" ++ tag ++ "' not present in module")
