{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.GenericInterpreter
where

import Prelude hiding (const, lookup)
import qualified Data.Map as M

import Interp.SharedHO.Data.BoolVal
import Interp.SharedHO.Data.Types

data Expr
    = Branch Int
    | Block Type Expr
    | Loop Type Expr
    | Seq [Expr]
    | Const Value
    | Add
    | Lt
    | Eqz
    | If Type Expr Expr
    | Assign String
    | Var String
    | Nop
    | Call String
    | Return

data Func = Func { funcParams :: [(String, Type)]
                 , funcRetType :: Type
                 , funcBody :: Expr }

type ToyModule = [(String, Func)]

data BlockType
    = BackwardJump
    | ForwardJump
    deriving (Show, Eq)

class Monad m => Interp m v | m -> v where
    pushBlock :: Type -> BlockType -> m () -> m () -> m ()
    popBlock :: Int -> m ()
    const :: Value -> m v
    add :: v -> v -> m v
    lt :: v -> v -> m v
    eqz :: v -> m v
    if_ :: v -> m () -> m () -> m ()
    assign :: String -> v -> m ()
    lookup :: String -> m v
    push :: v -> m ()
    pop :: m v
    assignFunc :: String -> m () -> m () -> m ()
    call :: String -> m ()
    closure :: Type -> m () -> m ()
    return_ :: m ()

class Monad m => Fix m where
    fix :: (m () -> m ()) -> m ()

interp :: (Interp m a, Fix m) => Expr -> m ()
interp expr = case expr of
    Branch n -> popBlock n

    Block rty e -> pushBlock rty ForwardJump (return ()) (interp e)

    Loop rty e -> fix $ \br -> pushBlock rty BackwardJump br (interp e)

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

    Eqz -> do
        v1 <- pop
        v2 <- eqz v1
        push v2

    If rty t f -> do
        c <- pop
        let t' = pushBlock rty ForwardJump (return ()) (interp t)
        let f' = pushBlock rty ForwardJump (return ()) (interp f)
        if_ c t' f'

    Assign var -> do
        v <- pop
        assign var v

    Var var -> do
        v <- lookup var
        push v

    Nop -> return ()

    Call name -> call name

    Return -> return_


interpFunc :: (Interp m v, Fix m) => ToyModule -> String -> [v]-> m ()
interpFunc mdl startName vs =
    let getArgs f = mapM (\(var, _) -> do { v <- pop; return (var, v) })
            (funcParams f)

        assignArgs = mapM_ (\(var, v) -> assign var v)

        firstCall name f = fix $ \recCall -> do
                assignFunc name recCall $ do
                    args <- getArgs f
                    closure (funcRetType f) $ do
                        assignArgs args
                        interp $ funcBody f

        start = do
            mapM_ push vs
            call startName

    in  foldl (\m (name, f) -> assignFunc name (firstCall name f) m) start mdl
