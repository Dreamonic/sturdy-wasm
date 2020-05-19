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

data CheckerState = CheckerState {
    _variables :: M.Map String CType,
    _checkType :: CheckerType
} deriving (Show, Eq)

emptyTypeCheckState = CheckerState M.empty (CheckerType Mid [])

makeLenses ''CheckerState
makeLenses ''CheckerType

stackTyping' :: Lens' CheckerState StackType
stackTyping' = checkType . stackTyping

stack' :: Lens' CheckerState [CType]
stack' = checkType . stack

type CheckerFuncMap = M.Map String (TypeChecker ())

newtype TypeChecker a = TypeChecker
    { runTypeChecker :: ExceptT TException (StateT CheckerFuncMap 
        (ReaderT [Maybe Type] (State CheckerState))) a}
    deriving (Functor, Applicative, Monad, MonadReader [Maybe Type],
        MonadState CheckerFuncMap, MonadError TException)

checkStateLift :: State CheckerState a -> TypeChecker a
checkStateLift m = TypeChecker . lift . lift $ lift m

getSt :: TypeChecker CheckerState
getSt = checkStateLift get

getsSt :: (CheckerState -> a) -> TypeChecker a
getsSt f = checkStateLift $ gets f

putSt :: CheckerState -> TypeChecker ()
putSt st = checkStateLift $ put st

modifySt :: (CheckerState -> CheckerState) -> TypeChecker ()
modifySt f = checkStateLift $ modify f

instance Interp TypeChecker CType where
    pushBlock rty blockType _ adv = do
        outerBlockState <- getSt
        putSt $ set stack' [] outerBlockState
        if blockType == BackwardJump
            then local (Nothing :) adv
            else local (Just rty :) adv
        val <- pop
        case val of
            SomeT val' | val' /= rty -> throwError $ TypeMismatch rty val'
            _                        -> return ()
        putSt $ over stack' (SomeT rty :) outerBlockState

    popBlock n = do
        rtys <- asks (drop n)
        st   <- getSt
        let stack'' = view stack' st
        if null rtys
            then throwError $ InvalidDepth n (length rtys)
            else do
                let rty = head rtys
                when (isJust rty) $ do
                    val <- pop
                    case (val, rty) of
                        (SomeT val', Just rty') | val' == rty' ->
                            putSt $ over stack' tail st
                        (AnyT, Just _) -> putSt $ over stack' tail st
                        _              -> throwError $ TypeMismatch rty stack''
                modifySt (set stackTyping' Top)
                modifySt (set stack' [])

    const = return . SomeT . getType

    add t1 t2 =
        if t1 /= t2 then throwError (InvalidOp "add" t1 t2) else return t1

    lt t1 t2 = if t1 /= t2
        then throwError (InvalidOp "compare" t1 t2)
        else return $ SomeT I32

    eqz _ = return $ SomeT I32

    if_ rty t f = do
        st <- getSt
        t
        stack1 <- getsSt (view stack')
        when (head stack1 /= rty) $ throwError $ TypeMismatch rty (head stack1)
        putSt st
        f
        stack2 <- getsSt (view stack')
        when (head stack2 /= rty) $ throwError $ TypeMismatch rty (head stack2)
        putSt $ over stack' (rty :) st

    assign var v = do
        st <- getSt
        let expected = M.lookup var $ view variables st
        case expected of
            Just t  -> when (t /= v) $ throwError $ InvalidAssignment v var t
            Nothing -> putSt $ over variables (M.insert var v) st

    lookup var = do
        st <- getSt
        case M.lookup var (view variables st) of
            Just v  -> return v
            Nothing -> throwError $ NotInScope var

    push v = modifySt $ over stack' (v :)

    pop = do
        st <- getSt
        case view stack' st of
            v : _ -> do
                modifySt $ over stack' tail
                return v
            [] -> if view stackTyping' st == Top
                then return AnyT
                else throwError StackUnderflow

    assignFunc name f adv = do
        funcMap <- get
        unless (M.member name funcMap) $ modify (M.insert name f)
        adv

    call name = do
        fs <- get
        case M.lookup name fs of
            Just f  -> f
            Nothing -> throwError $ NameError name

    closure rty m = do
        st <- getSt
        putSt emptyTypeCheckState
        pushBlock rty ForwardJump (return ()) m
        v <- pop
        putSt st
        push v

    return_ = do
        blockCount <- asks length
        popBlock $ blockCount - 1

instance Fix () TypeChecker where
    fix _ f = f (return ())

runTC :: Expr -> (Either TException (), CheckerState)
runTC e = do
    let
        ((result, _), st) = runState
            (runReaderT
                (runStateT
                    (runExceptT
                        (runTypeChecker ((interp :: Expr -> TypeChecker ()) e))
                    )
                    M.empty
                )
                []
            )
            emptyTypeCheckState

    (result, st)

runFuncTC :: ToyModule -> String -> [CType] -> (Either TException (), CheckerState)
runFuncTC mdl name vs = do
    let
        ((result, _), st) = runState
            (runReaderT
                (runStateT
                    (runExceptT
                        (runTypeChecker ((interpFunc :: ToyModule -> String -> [CType] -> TypeChecker ()) mdl name vs))
                    )
                    M.empty
                )
                []
            )
            emptyTypeCheckState

    (result, st)