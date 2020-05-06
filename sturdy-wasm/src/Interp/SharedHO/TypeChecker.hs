{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.TypeChecker where

import qualified Data.Map as M
import Prelude hiding (const, lookup)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Control.Monad.State hiding (fix, join, state)
import Control.Monad.Reader hiding (fix, join)
import Control.Monad.Except hiding (fix, join)
import Control.Monad.Writer hiding (fix, join)
import Control.Lens hiding (Const, assign)
import Control.Lens.TH

import Interp.SharedHO.Data.Joinable
import Interp.SharedHO.Data.BoolVal
import Interp.SharedHO.Data.Types
import Interp.SharedHO.GenericInterpreter
import Interp.SharedHO.Exceptions

data CType
    = AnyT
    | SomeT Type
    deriving (Show)

instance Eq CType where
    SomeT x == SomeT y = x == y
    _       == _       = True

data StackType = Top | Mid deriving (Show, Eq)

data CheckerType = CheckerType {
    _stackTyping :: StackType,
    _stack :: [CType]
} deriving (Show, Eq)

data TypeCheckState = TypeCheckState {
    _variables :: M.Map String CType,
    _checkType :: CheckerType
} deriving (Show, Eq)

emptyTypeCheckState = TypeCheckState M.empty (CheckerType Mid [])

makeLenses ''TypeCheckState
makeLenses ''CheckerType

stackTyping' :: Lens' TypeCheckState StackType
stackTyping' = checkType . stackTyping

stack' :: Lens' TypeCheckState [CType]
stack' = checkType . stack

newtype TypeChecker a = TypeChecker
    { runTypeChecker :: ExceptT TException (ReaderT [Maybe Type] (State TypeCheckState)) a}
    deriving (Functor, Applicative, Monad, MonadReader [Maybe Type],
        MonadState TypeCheckState, MonadError TException)

instance Interp TypeChecker CType where
    pushBlock rty blockType _ adv = do
        outerBlockState <- get
        put $ set stack' [] outerBlockState
        if blockType == BackwardJump
            then local (Nothing :) adv
            else local (Just rty :) adv
        val <- pop
        case val of
            SomeT val' | val' /= rty -> throwError $ TypeMismatch rty val'
            _                        -> return ()
        put $ over stack' (SomeT rty :) outerBlockState

    popBlock n = do
        rtys <- asks (drop n)
        st   <- get
        let stack'' = view stack' st
        if null rtys
            then throwError $ InvalidDepth n (length rtys)
            else do
                let rty = head rtys
                when (isJust rty) $ do
                    val <- pop
                    case (val, rty) of
                        (SomeT val', Just rty') | val' == rty' ->
                            put $ over stack' tail st
                        (AnyT, Just _) -> put $ over stack' tail st
                        _              -> throwError $ TypeMismatch rty stack''
                modify (set stackTyping' Top)
                modify (set stack' [])

    const = return . SomeT . getType

    add t1 t2 =
        if t1 /= t2 then throwError (InvalidOp "add" t1 t2) else return t1

    lt t1 t2 = if t1 /= t2
        then throwError (InvalidOp "compare" t1 t2)
        else return $ SomeT I32

    eqz _ = return $ SomeT I32

    if_ rty t f = do
        st <- get
        t
        stack1 <- gets (view stack')
        put st
        f
        stack2 <- gets (view stack')
        if head stack1 == rty && head stack2 == rty
            then put $ over stack' (rty :) st
            else throwError $ TypeMismatch (head stack1) (head stack2)

    assign var v = do
        st <- get
        let expected = M.lookup var $ view variables st
        case expected of
            Just t  -> when (t /= v) $ throwError $ InvalidAssignment v var t
            Nothing -> put $ over variables (M.insert var v) st

    lookup var = do
        st <- get
        case M.lookup var (view variables st) of
            Just v  -> return v
            Nothing -> throwError $ NotInScope var

    push v = modify $ over stack' (v :)

    pop = do
        st <- get
        case view stack' st of
            v : _ -> do
                modify $ over stack' tail
                return v
            [] -> if view stackTyping' st == Top
                then return AnyT
                else throwError StackUnderflow

instance Fix () TypeChecker where
    fix _ f = let x = f x in x

typecheck :: Expr -> (Either TException (), TypeCheckState)
typecheck e = runState
    (runReaderT
        (runExceptT
            (runTypeChecker
                ((interp :: Expr -> TypeChecker ()) e))) []) emptyTypeCheckState
