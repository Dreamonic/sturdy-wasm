{-# LANGUAGE TemplateHaskell #-}
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
    , label
    , code
    , initConfig
    , unEnv
    , push
    , pop
    , setVar
    , getVar
    , putInstr
    , putInstrList
    , putBlock
    , getInstr
    , hasInstr
    , retrieveStack
    , getStack
    , branchUp
    , endLabel
    , throwError
) where

import qualified Data.Map as Map
import Parser
import Debug.Trace
import Control.Lens

data ModInst =
    EmptyInst
    deriving (Show, Eq)
  
type Locals = Map.Map String WasmVal

data Frame =
    FrameT {_modInst :: ModInst, _locals :: Locals}
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
    f <*> v = Env (\n ->    let (val1, n1) = (unEnv f) n
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

code :: [Instr] -> Code
code es = Code [] (fmap Plain es)

-- |    Initialize the Config to Config with no locals, 
--      an empty value stack, and an empty execution stack.
initConfig :: Stack WasmVal -> AdminInstr -> Config
initConfig vs e = Config (FrameT EmptyInst Map.empty) (Code [] [label 1 (Code vs [e])])

-- |    Unencapsulate the state function.
unEnv :: MExecutor a -> (Config -> (Either String a, Config))
unEnv (Env e) = e

-- |    Get the stack from a Config.
getStack :: Config -> Stack WasmVal
getStack = view (confCode . stack)

-- |    Set the stack of a Config.
setStack :: Stack WasmVal -> Config -> Config
setStack vs conf= set (confCode . stack) vs conf

-- |    Push a single WasmVal to the top of the stack of a Config.
pushToStack :: WasmVal -> Config-> Config
pushToStack v = over (confCode . stack) ((:) v)

-- |    Pop the top most value from the innermost stack of a Config.
popFromStack :: Config -> (Either String WasmVal, Config)
popFromStack conf = case getStack conf of
    [] -> (Left "Cannot pop an empty stack", conf)
    v:vs -> (Right v, setStack vs conf)

popFromInstr :: Config -> (Either String AdminInstr, Config)
popFromInstr conf = case view (confCode . instr) conf of
    [] -> (Left "Cannot pop an empty instruction stack", conf)
    l ->    let (i, is) = findInnerInstr l
            in traceShow (i, view confCode conf) $ (Right i, set (confCode . instr) is conf)

findInnerInstr :: [AdminInstr] -> (AdminInstr, [AdminInstr])
findInnerInstr instr = case instr of
    (Label _ _ (Code _ is)):_ -> findInnerInstr is
    v:vs -> (v, vs)

removeLabel :: AdminInstr -> [AdminInstr]-> AdminInstr
removeLabel (Label n is (Code vs code)) rep = case code of 
    (Label n' is' (Code vs' [])):cs -> Label n is (Code ((take n' vs')++vs) cs)
    _ -> Label n is (Code vs rep)

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

putBlock :: Block -> MExecutor ()
putBlock (Block _ is) = traceShow (label 1 (Code [] (fmap Plain is))) putInstr $ label 1 (Code [] (fmap Plain is))

hasInstr :: MExecutor Bool
hasInstr = Env (\conf -> 
    let config = removeEmptyLabels conf in
    case view (confCode . instr) config of
        [] -> (Right False, config)
        _ -> (Right True, config))

removeEmptyLabels :: Config -> Config
removeEmptyLabels conf = case view (confCode . instr) conf of
    [] -> conf
    label:is -> let pos = findPosition label
                in case getFromPosition pos label of
                    Nothing | pos <= 0 -> set (confCode . instr) [] conf
                    Nothing -> removeEmptyLabels $ set (confCode. instr) ((removeAtPosition (pos-1) label):is) conf
                    Just _ -> conf

-- |    Retrieve the top most value of the outer stack.
retrieveStack :: MExecutor (Stack WasmVal)
retrieveStack = Env(\config -> (Right (view (confCode . stack) config), config))

-- |    Find the position of the most nested label, which can be seen
--      as the label that is currently being processed.
--      
--      [@instr@] The main instruction, which in most cases will be the outermost label.
findPosition :: AdminInstr -> Integer
findPosition instr = case instr of 
    (Label _ _ (Code _ (i:is))) -> 1 + (findPosition i)
    (Label _ _ (Code _ [])) -> 1
    _ -> 0

-- |    Get the label at the position indicated by the integer.
--
--      [@n@]       An integer indicating the position.
--      [@instr@]   The main instruction, which in most cases will be the outermost label.
goToPosition :: Integer -> AdminInstr -> AdminInstr
goToPosition n instr = case n of
    1 -> instr
    _ ->    let Label _ _ (Code _ (i:is)) = instr
            in goToPosition (n-1) i

-- |    Get the instruction at the top of the instruction stack from
--      the label indicated by the position. It might return nothing, 
--      if the instruction stack is empty.
--
--      [@n@]       An integer indicating the position.
--      [@instr@]   The main instruction, which in most cases will be the outermost label.
getFromPosition :: Integer -> AdminInstr -> Maybe AdminInstr
getFromPosition n instr = 
    let Label _ _ (Code _ c) = goToPosition n instr
    in case c of 
        [] -> Nothing
        c':cs -> Just c'

-- |    Replace the instruction at the top of the instruction stack from
--      the label indicated by the position
--
--      [@n@]           An integer indicating the position.
--      [@newInstr@]    The instruction by which the top of the instruction stack of
--                      the indicated label should be replaced with.
--      [@instr@]       The main instruction, which in most cases will be the outermost label.     
replaceAtPosition :: Integer -> AdminInstr -> AdminInstr -> AdminInstr
replaceAtPosition n newInstr instr = case n of
    0 -> newInstr
    _ ->    let Label rets ends (Code vs (i:is)) = instr
            in Label rets ends (Code vs ((replaceAtPosition (n-1) newInstr i):is))

-- |    Replace the instruction at the top of the instruction stack from
--      the label indicated by the position
--
--      [@n@]       An integer indicating the position.
--      [@rets@]    The number of return types a label has.
--      [@ends@]    The instructions which will be executed when @branch 0@ is executed.
--      [@vs@]      The value stack of the current label.
--      [@i@]       The instruction at the top of the stack.
--      [@is@]      The other instructions on the stack.
removeAtPosition :: Integer -> AdminInstr -> AdminInstr
removeAtPosition n (Label rets ends (Code vs (i:is))) = case n of
    1 -> Label rets ends (Code vs is)
    _ -> Label rets ends (Code vs ((removeAtPosition (n-1) i):is))

-- |    Find a local variable from local environment.
findLocal :: String -> Locals -> Either String WasmVal
findLocal id locals = case Map.lookup id locals of
  Just v -> Right v
  Nothing -> Left ("The id: '" ++ id ++ "' was not bound")

branchUp :: MExecutor ()
branchUp = Env (\config -> (Right (), config))

endLabel :: MExecutor ()
endLabel = Env (\config -> (Right (), config))

throwError :: String -> MExecutor ()
throwError err = Env (\config -> (Left err, config))