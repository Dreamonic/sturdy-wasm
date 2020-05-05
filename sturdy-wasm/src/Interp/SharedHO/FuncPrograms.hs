module Interp.SharedHO.FuncPrograms
where

import Interp.SharedHO.GenericInterpreter
import Interp.SharedHO.ConcreteInterpreter
import Interp.SharedHO.Data.Types

i32Val = Value I32
i64Val = Value I64

addition :: Func
addition = Func [("x", I32), ("y", I32)] I32 $
    Seq [Var "x", Var "y", Add]

lessThan :: Func
lessThan = Func [("x", I32), ("y", I32)] I32 $
    Seq [Var "x", Var "y", Lt]

ifStatement :: Func
ifStatement = Func [("x", I32)] I32 $
    Seq
        [ Const (i32Val 0)
        , Assign "y"
        , Var "x"
        , If I32
             (Seq [Const (i32Val 2), Assign "y", Const (i32Val 0)])
             (Seq [Const (i32Val 3), Assign "y", Const (i32Val 0)])
        , Var "y"
        ]

gaussSum :: Func
gaussSum = Func [("n", I32)] I32 $
    Seq
        [ Var "n"
        , Eqz
        , If I32
            (Const (i32Val 0))
            (Seq
                [ Var "n"
                , Var "n"
                , Const (i32Val (-1))
                , Add
                , Call "gauss"
                , Add
                ])
        ]

generalMdl :: ToyModule
generalMdl = [ ("add", addition)
             , ("ifStat", ifStatement)
             , ("gauss", gaussSum)
             , ("lt", lessThan)
             ]
