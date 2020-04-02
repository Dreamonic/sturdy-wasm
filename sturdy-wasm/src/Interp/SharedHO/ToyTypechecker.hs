{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.ToyTypechecker
where

import qualified Data.Map as M
import Prelude hiding (const, lookup)
import Control.Monad.State hiding (fix, join, state)
import Control.Monad.Reader hiding (fix, join)
import Control.Monad.Except hiding (fix, join)
import Control.Monad.Writer hiding (fix, join)
import Control.Lens hiding (Const, assign)
import Control.Lens.TH

import Interp.SharedHO.Joinable
import Interp.SharedHO.BoolVal

data Type
    = I32
    | I64
    deriving (Show, Eq)

data Value = Value {
    getType :: Type,
    getVal :: Integer
} deriving (Show, Eq)

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

equalType :: Value -> Value -> Bool
equalType v1 v2 = (getType v1) == (getType v2)

data TypeCheckState = TypeCheckState {
    _variables :: M.Map String Type,
    _stack :: [Maybe Type],
    _unreachable :: Bool
} deriving (Show, Eq)

makeLenses ''TypeCheckState

newtype TypeChecker a = TypeChecker
    { runTypeChecker :: WriterT [String] (ReaderT [Type] (State TypeCheckState)) a}
    deriving (Functor, Applicative, Monad, MonadReader [Type], 
        MonadState TypeCheckState, MonadWriter [String])

unexpectedType expected actual =
    ["Expected " ++ (show expected) ++ " but got " ++ (show actual)]

invalidOp op t1 t2 = 
    ["Cannot " ++ (show op) ++ " " ++ (show t1) ++ " and " ++ (show t2)]

instance Interp TypeChecker Type where
    pushBlock rty _ adv = do
        outerBlockState <- get
        put $ set stack [] outerBlockState
        local (rty :) adv
        innerBlockState <- get
        let innerStack = view stack innerBlockState
        unless (view unreachable innerBlockState || (head innerStack) == Just rty)
            $ tell (unexpectedType rty (head innerStack))
        put $ over stack (Just rty :) outerBlockState
        -- if view unreachable innerBlockState || (head innerStack) == rty
        --     then put $ over stack (rty :) outerBlockState
        --     else do
                -- tell
                --     [ "Expected "
                --       ++ (show rty)
                --       ++ " but got "
                --       ++ (show . head $ innerStack)
                --     ]
        --         put outerBlockState

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
                -- if not (null stack') && head stack' == rty
                --     then
                --     -- replace by pop
                --          put $ over stack tail st
                --     else
                --         tell $ unexpectedType rty stack'

    const = return . getType

    -- add t1 t2 = do
    --     unless (t1 == t2) $ tell (invalidOp "add" t1 t2)
    --     return

    -- lt t1 t2 = do
    --     unless (t1 == t2) $ tell (invalidOp "compare" t1 t2)

    if_ _ t f = do
        st <- get
        t
        put st
        f
        put st

    assign var v = do
        st <- get
        put $ over variables (M.insert var v) st

    -- lookup var = do
    --     st <- get
    --     case M.lookup var (view variables st) of
    --         Just v  -> return v
    --         Nothing -> tell ["Var " ++ var ++ " not in scope"]

    push v = modify $ over stack (Just v :)

    -- pop = do
    --     st <- get
    --     case view stack st of
    --         v:_ -> do
    --             modify $ over stack tail
    --             return v
    --         []  -> tell ["Tried to pop value from empty stack."]


-- newtype TypeChecker a = TypeChecker
--     { runTypeChecker :: ExceptT (Either [String] Int) (State TypeCheckState) a}
--     deriving (Functor, Applicative, Monad, MonadError (Either String Int), 
--         MonadState TypeCheckState)

-- instance Interp TypeChecker Type where
--     pushBlock rty br adv = do
--         outerBlockState <- get
--         put $ set stack [] outerBlockState
--         catchError adv $ \e -> case e of
--             Right n  -> if n <= 0
--                 then return ()
--                 else throwError $ Right $ n - 1
--             Left msg -> throwError $ Left msg
--         innerBlockState <- get
--         let innerStack = view stack innerBlockState
--         if (head innerStack) == rty then
--             put $ over stack (rty:) outerBlockState
--         else
--             throwError $ Left $ "Expected " ++ (show rty) ++ " but got " ++ (show . head $ innerStack)

--     popBlock n = throwError $ Right n

--     const = return . getType

--     add t1 t2 = do
--         if t1 == t2 then
--             return t1
--         else
--             throwError $ Left $ "Cannot add " ++ (show t1) ++ " and " ++ (show t2) 

--     lt t1 t2 = do
--         if t1 == t2 then
--             return t1
--         else
--             throwError $ Left $ "Cannot compare " ++ (show t1) ++ " with " ++ (show t2)

--     if_ t f = do
--         st <- get
--         t
--         put st
--         f
--         put st

--     assign var v = do
--         st <- get
--         put $ over variables (M.insert var v) st

--     lookup var = do
--         st <- get
--         case M.lookup var (view variables st) of
--             Just v  -> return v
--             Nothing -> throwError $ Left $ "Var " ++ var ++ " not in scope."

--     push v = modify $ over stack (v:)

--     pop = do
--         st <- get
--         case view stack st of
--             v:_ -> do
--                 modify $ over stack tail
--                 return v
--             []  -> throwError $ Left $ "Tried to pop value from empty stack."

-- instance Fix (TypeChecker ()) where
--     fix f = f (fix f)