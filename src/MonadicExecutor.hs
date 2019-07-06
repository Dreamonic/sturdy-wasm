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

-- |    Get the stack of the innermost label (active label).
--
--      [@conf@] The config containg all the necessary information.
getInnerStack :: Config -> Stack WasmVal
getInnerStack conf =
    case view (confCode . instr) conf of
        [] -> getStack conf
        i:_ -> 
            let n = findPosition i in                           -- Find the position of the innermost label
            case n of 
                0 -> getStack conf
                _ ->    let Label _ _ (Code vs _) = goToPosition n i in     -- Go to the innermost label
                        vs                                                  -- Retrieve the stack

-- |    Set the stack of a Config.
setStack :: Stack WasmVal -> Config -> Config
setStack vs conf= set (confCode . stack) vs conf

-- |    Set the stack of the innermost label (active label).
--
--      [@conf@] The config containg all the necessary information.
setInnerStack :: Stack WasmVal -> Config -> Config
setInnerStack vs conf = 
    case view (confCode . instr) conf of
        [] -> setStack vs conf
        i:is ->
            let n = findPosition i in                                               -- Find the position of the innermost label
            let Label rets ends (Code _ iss) = goToPosition n i in                  -- Go to the innermost label
            let i' = replaceAtPosition (n-1) (Label rets ends (Code vs iss)) i in   -- Exchange the old stack with the new stack
                set (confCode . instr) (i':is) conf                                 -- Return the new config with the updated stack

-- |    Push a single WasmVal to the top of the stack of a Config.
pushToStack :: WasmVal -> Config-> Config
pushToStack v = over (confCode . stack) ((:) v)

-- |    Push a value to the top of the stack of the innermost label.
--
--      [@v@]       The value that should be pushed to the top of the stack.
--      [@conf@]    The old configuration containing all necessary information.
pushToInnerStack :: WasmVal -> Config -> Config
pushToInnerStack v conf = setInnerStack (v:(getInnerStack conf)) conf

-- |    Push a value to the top of the stack of the innermost label.
--
--      [@vs@]       The value that should be pushed to the top of the stack.
--      [@conf@]    The old configuration containing all necessary information.
pushStackToInnerStack :: Stack WasmVal -> Config -> Config
pushStackToInnerStack vs conf = setInnerStack (vs++(getInnerStack conf)) conf

-- |    Pop the top most value from the innermost stack of a Config.
popFromStack :: Config -> (Either String WasmVal, Config)
popFromStack conf = case getStack conf of
    [] -> (Left "Cannot pop an empty stack", conf)
    v:vs -> (Right v, setStack vs conf)

-- |    Pop the top value from the stack of the innermost label.
--
--      [@conf@]    The old configuration containing all necessary information.
popFromInnerStack :: Config -> (Either String WasmVal, Config)
popFromInnerStack conf = case getInnerStack conf of
    [] -> (Left "Cannot pop an empty stack", conf)
    v:vs -> (Right v, setInnerStack vs conf)

popFromInstr :: Config -> (Either String AdminInstr, Config)
popFromInstr conf = case view (confCode . instr) conf of
    [] -> (Left "Cannot pop an empty instruction stack", conf)
    i:is -> let n = findPosition i in
            let Just ret = getFromPosition n i in
            let i' = removeAtPosition n i in
                (Right ret, set (confCode . instr) (i':is) conf)

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
push v = Env (\config -> (Right (), (pushToInnerStack v config)))

-- |    Pop a single WasmVal from the value stack.
pop :: MExecutor WasmVal
pop = Env (\config -> popFromInnerStack config)

-- |    Put a value into the local environment
setVar :: String -> WasmVal -> MExecutor () 
setVar id v = Env (\config -> (Right (), over (confFrame . locals) (Map.insert id v) config))

-- |    Get a value from the local environment specified by the variable.
getVar :: String -> MExecutor WasmVal
getVar id = Env (\config -> (findLocal id (view (confFrame . locals) config), config))

-- |    Put an instruction on the execution stack.
putInstr :: AdminInstr -> MExecutor ()
putInstr ins = Env (\config -> (Right (), 
    let i:is = view (confCode . instr) config in
    let n = findPosition i in
    let (Label rets ends (Code vs is)) = goToPosition n i in
    let i' = replaceAtPosition (n-1) (Label rets ends (Code vs (ins:is))) i in
        set (confCode . instr) (i':is) config))

-- |    Put a list of instructions on the execution stack.
putInstrList :: [AdminInstr] -> MExecutor ()
putInstrList is = Env (\config -> (Right (), over (confCode . instr) ((++) is) config))

-- |    Get the first instruction from the execution stack.
getInstr :: MExecutor AdminInstr
getInstr = Env (\config -> popFromInstr config)

putBlock :: Block -> MExecutor ()
putBlock (Block _ is) = putInstr $ label 1 (Code [] (fmap Plain is))

-- |    Check whether there are more instructions to be executed.
hasInstr :: MExecutor Bool
hasInstr = Env (\conf -> 
    let config = removeEmptyLabels conf in      -- Remove all labels with no instructions left
    case view (confCode . instr) config of      -- Retrieve the instructions
        [] -> (Right False, config)             -- If there are no more instructions, return false
        _ -> (Right True, config))              -- otherwise return true

-- |    Remove all empty labels recursively, or do nothing whenever
--      there are still instructions left within the label.
--      This does not add the end instructions to the instruction stack of the outer label.
removeEmptyLabels :: Config -> Config
removeEmptyLabels conf = case view (confCode . instr) conf of
    [] -> conf
    label:is -> let pos = findPosition label in                                                     -- Find the position of the innermost label
                let Label rets _ (Code vs _) = goToPosition pos label in
                let stack = take rets vs in
                    case getFromPosition pos label of                                               -- Retrieve the instruction from the label
                        Nothing | pos <= 1 -> removeEmptyLabels $ 
                            pushStackToInnerStack stack
                                (set (confCode . instr) is conf)    -- If there are no instructions, remove it
                        Nothing -> removeEmptyLabels $ 
                            pushStackToInnerStack stack 
                                (set (confCode . instr) ((removeAtPosition (pos - 1) label):is) conf)
                        Just _ -> conf                                                              -- otherwise do nothing

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
replaceAtPosition n newInstr instr = 
    case n of
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
removeAtPosition n (Label rets ends (Code vs (i:is))) =
    case n of
        1 -> Label rets ends (Code vs is)
        _ -> Label rets ends (Code vs ((removeAtPosition (n-1) i):is))

-- |    Find a local variable from local environment.
findLocal :: String -> Locals -> Either String WasmVal
findLocal id locals = case Map.lookup id locals of
  Just v -> Right v
  Nothing -> Left ("The id: '" ++ id ++ "' was not bound")

branchUp :: MExecutor ()
branchUp = Env (\config -> 
    case view (confCode . instr) config of
        [] -> (Left "Cannot branch up more than the root", config)
        i:is -> 
            let n = findPosition i in
            let Label rets ends (Code vs (i':is')) = goToPosition (n-1) i in
            let Label r2 _ (Code vs2 _) = goToPosition n i in
            let i'' = replaceAtPosition (n-2) (Label rets ends (Code ((take r2 vs2)++vs) is')) i in
                (Right (), set (confCode . instr) (i'':is) config))

endLabel :: MExecutor ()
endLabel = Env (\config -> case view (confCode . instr) config of
    [] -> (Left "Cannot branch up more than the root", config)
    i:is -> 
        let n = findPosition i in
        let Label rets ends (Code vs (i':is')) = goToPosition (n-1) i in
        let Label r2 e2 (Code vs2 _) = goToPosition n i in
        let i'' = replaceAtPosition (n-2) (Label rets ends (Code ((take r2 vs2)++vs) ((fmap Plain e2)++is'))) i in
            (Right (), set (confCode . instr) (i'':is) config))

throwError :: String -> MExecutor ()
throwError err = Env (\config -> (Left err, config))