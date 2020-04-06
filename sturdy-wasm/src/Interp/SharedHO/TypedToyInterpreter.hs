{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.TypedToyInterpreter
where

import qualified Data.Map as M
import Prelude hiding (const, lookup)
import Data.List (intercalate)
import Control.Monad.State hiding (fix, join, state)
import Control.Monad.Reader hiding (fix, join)
import Control.Monad.Except hiding (fix, join)
import Control.Monad.Writer hiding (fix, join)
import Control.Lens hiding (Const, assign)
import Control.Lens.TH

import Interp.SharedHO.Joinable
import Interp.SharedHO.BoolVal
import Interp.SharedHO.Types

data Expr
    = Branch Int
    | Block Type Expr
    | Loop Type Expr
    | Seq [Expr]
    | Const Value
    | Add
    | Lt
    | If Type Expr Expr
    | Assign String
    | Var String
    | Nop

instance ToBool Integer where
    toBool = (/=) 0

instance FromBool Integer where
    fromBool b = if b then 1 else 0

class Monad m => Interp m v | m -> v where
    pushBlock :: Type -> m () -> m () -> m ()
    popBlock :: Int -> m ()
    const :: Value -> m v
    add :: v -> v -> m v
    lt :: v -> v -> m v
    if_ :: v -> m () -> m () -> m ()
    assign :: String -> v -> m ()
    lookup :: String -> m v
    push :: v -> m ()
    pop :: m v

class Fix c where
    fix :: (c -> c) -> c

interp :: (Interp m a, Fix (m ())) => Expr -> m ()
interp expr = case expr of
    Branch n -> popBlock n

    Block rty e -> pushBlock rty (return ()) (interp e)

    Loop rty e -> fix $ \br -> pushBlock rty br (interp e)

    Seq es -> sequence_ (interp <$> es)

    Const n -> do
        v <- const n
        push v

    Add -> do
        v1 <- pop
        v2 <- pop
        v3 <- add v2 v1
        push v3

    Lt -> do
        v1 <- pop
        v2 <- pop
        v3 <- lt v2 v1
        push v3

    If rty t f -> do
        c <- pop
        let t' = pushBlock rty (return ()) (interp t)
        let f' = pushBlock rty (return ()) (interp f)
        if_ c t' f'

    Assign var -> do
        v <- pop
        assign var v

    Var var -> do
        v <- lookup var
        push v

    Nop -> return ()


-- locals are scoped by blocks 
-- so a local that is assigned in an inner block should not
-- affect the outer block

-- it is assumed that the type of locals can not change

-- it is assumed that blocks should typecheck under context
-- but can not affect the state of surrounding blocks

-- Interesting notes

-- if you want to use multiple exceptions, 
-- meaning that checking continues after an error is detected
-- the result of operations that are invalid can be unknown
-- you can do 3 things in these situations I think
-- 1. Assume the operations following the invalid operation get what they expect
--    using something like an "Unknown" value which always typechecks
-- 2. Make certain operations typed, such as add. This ensures that you can throw
--    the error and just push whatever you where expecting to be on the stack
--    this is the case for blocks in webassembly 
-- 3. Stop typechecking entirely and tell the user to fix this problem
--    before moving on to typechecking the rest

-- there are 2 ways you can handle unconditional jumping
-- 1. ignoring all code after the jump, assuming that it is unreachable
-- 2. typechecking it based on the result of typechecking the branch, 
--    like it is done in webassembly

data TypeCheckState = TypeCheckState {
    _variables :: M.Map String MaybeType,
    _stack :: [MaybeType],
    _unreachable :: Bool
} deriving (Show, Eq)

emptyTypeCheckState = TypeCheckState M.empty [] False

makeLenses ''TypeCheckState

newtype TypeChecker a = TypeChecker
    { runTypeChecker :: WriterT [String] (ReaderT [Type] (State TypeCheckState)) a}
    deriving (Functor, Applicative, Monad, MonadReader [Type], 
        MonadState TypeCheckState, MonadWriter [String])

-- TODO find good definition for join
matchingReturn :: [MaybeType] -> [MaybeType] -> MaybeType
matchingReturn stack1 stack2 = case (stack1, stack2) of
    (v1:_, v2:_) -> v1 `join` v2
    _ -> Unknown
    
unexpectedType expected actual =
    ["Expected " ++ (show expected) ++ " but got " ++ (show actual)]

invalidOp op t1 t2 = 
    ["Cannot " ++ op ++ " " ++ (show t1) ++ " and " ++ (show t2)]

instance Interp TypeChecker MaybeType where
    pushBlock rty _ adv = do
        outerBlockState <- get
        put $ set stack [] outerBlockState
        local (rty :) adv
        innerBlockState <- get
        let innerStack = view stack innerBlockState
        let isUnreachable = view unreachable innerBlockState
        unless (isUnreachable) $ do
            if null innerStack
                then tell ["Cannot pop from empty stack"]
                else when ((head innerStack) /= Known rty)
                    $ tell (unexpectedType rty (head innerStack))
        put $ over stack (Known rty :) outerBlockState

    popBlock n = do
        rtys <- (drop n) <$> ask
        st   <- get
        let stack' = view stack st
        if null rtys
            then tell ["Can't break " ++ show n]
            else do
                let rty = head rtys
                case stack' of
                    rty:_ -> put $ over stack tail st
                    _ -> tell $ unexpectedType rty stack' 
                modify (set unreachable True)

    const = return . Known . getType

    add t1 t2 = do
        unless (t1 == t2) $ tell (invalidOp "add" t1 t2)
        return $ t1 `join` t2

    lt t1 t2 = do
        unless (t1 == t2) $ tell (invalidOp "compare" t1 t2)
        return $ Known I32

    if_ _ t f = do
        st <- get
        t
        stack1 <- view stack <$> get
        put st
        f
        stack2 <- view stack <$> get
        let rty = matchingReturn stack1 stack2
        put $ over stack (rty :) st

    assign var v = do
        st <- get
        let expected = M.lookup var $ view variables st
        case expected of
            (Just t) -> when (t /= v) $ 
                tell $ ["Cannot assign " ++ (show v) ++ " to " ++ (show t)]
            Nothing -> put $ over variables (M.insert var v) st

    lookup var = do
        st <- get
        case M.lookup var (view variables st) of
            Just v  -> return v
            Nothing -> do
                tell ["Var " ++ var ++ " not in scope"]
                return Unknown

    push v = modify $ over stack (v :)

    pop = do
        st <- get
        case view stack st of
            v:_ -> do
                modify $ over stack tail
                return v
            []  -> do
                tell ["Tried to pop value from empty stack."]
                return Unknown

instance Fix (TypeChecker ()) where
    fix f = f (fix f)

typecheck :: Expr -> (((), [String]), TypeCheckState)
typecheck e = 
    runState
        (runReaderT
            (runWriterT
                (runTypeChecker
                    ((interp :: Expr -> TypeChecker ()) e))) []) emptyTypeCheckState

typecheck' :: Expr -> IO ()
typecheck' e = do
    let result = typecheck e
    let state = snd result
    let errors = snd $ fst result
    putStrLn $ show state
    putStrLn $ intercalate "\n" (map ("[!] " ++) errors) 
