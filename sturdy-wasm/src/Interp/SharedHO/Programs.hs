{-# LANGUAGE DataKinds #-}

module Interp.SharedHO.Programs
where

import Interp.SharedHO.GenericInterpreter
import Interp.SharedHO.TypeCheckerSimple
import Interp.SharedHO.IntervalAnalysis
import Interp.SharedHO.ReachingDefinitions
import Interp.SharedHO.Data.Types

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

invalidIfStatement :: Expr
invalidIfStatement = Seq
    [ Const (i32Val 0)
    , Assign "y"
    , Const (i32Val 0)
    , If I32
         (Seq [Const (i64Val 2), Assign "y", Const (i32Val 0)])
         (Seq [Const (i32Val 0), Const (i64Val 1), Add])
    , Var "y"
    ]

invalidReturnType :: Expr
invalidReturnType = Seq
    [ Block I32 $ Seq [Const (i64Val 1), Branch 0]
    , Const (i32Val 1)
    , Add
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

infiniteLoop2 :: Expr
infiniteLoop2 = Seq
    [ Const (i32Val 0)
    , Assign "y"
    , Const (i32Val 8)
    , Assign "x"
    , Loop I32 $ Seq [ Const (i32Val 1), Var "y", Add, Assign "y"
                     , Const (i32Val 8), Const (i32Val 1), Add, Assign "x"
                     , Branch 0
                     ]
    , Var "x"
    ]

nestedBlock :: Expr
nestedBlock = Seq
    [ Block I32 $ Seq
        [ Block I32
            $ Seq [Const (i32Val 0), Branch 1, Const (i32Val 1), Add]
        , Const (i32Val 2)
        , Add
        ]
    , Const (i32Val 4)
    , Add
    ]

nestedLoop :: Expr
nestedLoop = Seq
    [ Const (i32Val 0)
    , Assign "x"
    , Loop I32 $ Seq
        [ Var "x"
        , Const (i32Val 5)
        , Lt
        , If
            I32
            (Seq
                [ Const (i32Val 1)
                , Var "x"
                , Add
                , Assign "x"
                , Const (i32Val 0)
                , Assign "y"
                , Loop I32 $ Seq
                    [ Var "y"
                    , Const (i32Val 5)
                    , Var "x"
                    , Add
                    , Lt
                    , If
                        I32
                        (Seq
                            [ Const (i32Val 1)
                            , Var "y"
                            , Add
                            , Assign "y"
                            , Branch 1
                            ]
                        )
                        (Const (i32Val 1))
                    ]
                , Branch 0
                ]
            )
            (Const (i32Val 1))
        ]
    ]

unreachableWellTyped = Block I32 $ Seq [Const (i32Val 1), Branch 0, Add]

unreachableIllTyped = Block I32 $ Seq
    [Const (i32Val 1), Branch 0, Const (i32Val 1), Const (i64Val 1), Add]

loopReturn = Loop I32 $ Seq [(Const (i64Val 1)), Branch 0]

blockReturn = Block I32 $ Seq [(Const (i64Val 1)), Branch 0]
