{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.Typed.TypeCheckerSimple
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
import Interp.SharedHO.Typed.Types
import Interp.SharedHO.Typed.GenericInterpreter

data CType
    = AnyT
    | SomeT Type
    deriving (Show)

instance Eq CType where
    a == b = case (a, b) of
        (SomeT a', SomeT b') -> a' == b'
        _ -> True

instance Joinable CType where
    join a b = case (a, b) of
        (SomeT a', SomeT b') | a' == b' -> a
        _ -> AnyT

data TypeCheckState = TypeCheckState {
    _variables :: M.Map String CType,
    _stack :: [CType],
    _is_top :: Bool
} deriving (Show, Eq)

emptyTypeCheckState = TypeCheckState M.empty [] False

makeLenses ''TypeCheckState

newtype TypeChecker a = TypeChecker
    { runTypeChecker :: ExceptT String (ReaderT [Maybe Type] (State TypeCheckState)) a}
    deriving (Functor, Applicative, Monad, MonadReader [Maybe Type], 
        MonadState TypeCheckState, MonadError String)

-- TODO find good definition for join
matchingReturn :: [CType] -> [CType] -> CType
matchingReturn stack1 stack2 = case (stack1, stack2) of
    (v1 : _, v2 : _) -> v1 `join` v2
    _                -> AnyT
    
unexpectedType expected actual =
    "Expected " ++ (show expected) ++ " but got " ++ (show actual)

invalidOp op t1 t2 = 
    "Cannot " ++ op ++ " " ++ (show t1) ++ " and " ++ (show t2)

instance Interp TypeChecker CType where
    pushBlock rty isLoop _ adv = do
        outerBlockState <- get
        put $ set stack [] outerBlockState
        if isLoop
            then local (Nothing :) adv
            else local (Just rty :) adv
        val <- pop
        case val of
            SomeT val' | val' /= rty -> throwError $ unexpectedType rty val'
            _                        -> return ()
        put $ over stack (SomeT rty :) outerBlockState

    popBlock n = do
        rtys <- drop n <$> ask
        st   <- get
        let stack' = view stack st
        if null rtys
            then throwError $ "Can't break " ++ show n
            else do
                let rty = head rtys
                when (rty /= Nothing) $ do
                    val <- pop
                    case (val, rty) of
                        (SomeT val', Just rty') | val' == rty' -> do
                            put $ over stack tail st
                        (AnyT, Just _) -> put $ over stack tail st
                        _              -> throwError $ unexpectedType rty stack'
                modify (set is_top True)
                modify (set stack [])

    const = return . SomeT . getType

    add t1 t2 = do
        if (t1 /= t2)
            then throwError (invalidOp "add" t1 t2)
            else return t1

    lt t1 t2 = do
        if (t1 /= t2)
            then throwError (invalidOp "compare" t1 t2)
            else return $ SomeT I32

    not_ = return

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
                throwError $ "Cannot assign " ++ (show v) ++ " to " ++ var ++ " of " ++ (show t)
            Nothing -> put $ over variables (M.insert var v) st

    lookup var = do
        st <- get
        case M.lookup var (view variables st) of
            Just v  -> return v
            Nothing -> throwError $ "Var " ++ var ++ " not in scope"

    push v = modify $ over stack (v :)

    pop = do
        st <- get
        case view stack st of
            v : _ -> do
                modify $ over stack tail
                return v
            [] -> do
                if (view is_top st)
                    then return AnyT
                    else throwError "Tried to pop value from empty stack."

instance Fix (TypeChecker ()) where
    fix f = f (fix f)

typecheck :: Expr -> (Either String (), TypeCheckState)
typecheck e = 
    runState
        (runReaderT
            (runExceptT
                (runTypeChecker
                    ((interp :: Expr -> TypeChecker ()) e))) []) emptyTypeCheckState