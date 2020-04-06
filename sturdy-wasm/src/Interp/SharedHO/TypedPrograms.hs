{-# LANGUAGE DataKinds #-}

module Interp.SharedHO.TypedPrograms
where

import Interp.SharedHO.TypedToyInterpreter
import Interp.SharedHO.Types

i32Val = Value I32
i64Val = Value I64

addition :: Expr
addition = Seq [Const (i32Val 2), Const (i32Val 5), Add]

additionInvalid :: Expr
additionInvalid = Seq [Const (i32Val 2), Const (i64Val 5), Add]

ifStatement :: Expr
ifStatement = Seq
    [ Const (i32Val 0)
    , Assign "y"
    , Const (i32Val 0)
    , If I32
         (Seq [Const (i32Val 2), Assign "y", Const (i32Val 0)])
         (Seq [Const (i32Val 3), Assign "y", Const (i32Val 0)])
    , Var "y"
    ]

ifStatementInvalid :: Expr
ifStatementInvalid = Seq
    [ Const (i32Val 0)
    , Assign "y"
    , Const (i32Val 0)
    , If I32
         (Seq [Const (i64Val 2), Assign "y", Const (i32Val 0)])
         (Seq [Const (i32Val 0), Const (i64Val 1), Add])
    , Var "y"
    ]

finiteLoop :: Expr
finiteLoop = Seq
    [ Const (i32Val 0)
    , Assign "x"
    , Loop I32 $ Seq
        [ Var "x"
        , Const (i32Val 5)
        , Lt
        , If I32
             (Seq [Const (i32Val 1), Var "x", Add, Assign "x", Branch 1, Var "x"])
             (Var "x")
        ]
    ]

infiniteLoop :: Expr
infiniteLoop = Seq
    [ Const (i32Val 0)
    , Assign "x"
    , Loop I32 $ Seq [Const (i32Val 1), Var "x", Add, Assign "x", Branch 0]
    , Var "x"
    ]