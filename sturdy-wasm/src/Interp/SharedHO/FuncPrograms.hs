module Interp.SharedHO.FuncPrograms
where

import Interp.SharedHO.GenericInterpreter
import Interp.SharedHO.ConcreteInterpreter
import Interp.SharedHO.ReachingDefinitions
import Interp.SharedHO.IntervalAnalysis
import qualified Interp.SharedHO.Data.RDSet as RD
import qualified Interp.SharedHO.Data.Interval as Interval
import Interp.SharedHO.Data.Value

rdVal = RD.singleton
iaVal = Interval.degenerate

addition :: Func
addition = Func [("x", I32), ("y", I32)] I32 $
    Seq [Var "x", Var "y", Add]

addition64 :: Func
addition64 = Func [("x", I64), ("y", I64)] I64 $
    Seq [Var "x", Var "y", Add]

lessThan :: Func
lessThan = Func [("x", I32), ("y", I32)] I32 $
    Seq [Var "x", Var "y", Lt]

ifStatement :: Func
ifStatement = Func [("x", I32)] I32 $
    Seq
        [ Const (I32Val 0)
        , Assign "y"
        , Var "x"
        , If I32
             (Seq [Const (I32Val 2), Assign "y", Const (I32Val 0)])
             (Seq [Const (I32Val 3), Assign "y", Const (I32Val 0)])
        , Var "y"
        ]

gaussSum :: Func
gaussSum = Func [("n", I32)] I32 $
    Seq
        [ Var "n"
        , Eqz
        , If I32
            (Const (I32Val 0))
            (Seq
                [ Var "n"
                , Var "n"
                , Const (I32Val (-1))
                , Add
                , Call "gauss"
                , Add
                , Return
                , Add
                , Add
                , Add
                ])
        ]

infiniteRecursion :: Func
infiniteRecursion = Func [] I32 $
    Call "rec"

evenRec :: Func
evenRec = Func [("n", I32)] I32 $
    Seq
        [ Var "n"
        , Eqz
        , If I32
            (Const (I32Val 1))
            (Seq
                [ Var "n"
                , Const (I32Val (-1))
                , Add
                , Call "odd"
                ])
        ]

oddRec :: Func
oddRec = Func [("n", I32)] I32 $
    Seq
        [ Var "n"
        , Eqz
        , If I32
            (Const (I32Val 0))
            (Seq
                [ Var "n"
                , Const (I32Val (-1))
                , Add
                , Call "even"
                ])
        ]

generalMdl :: ToyModule
generalMdl = [ ("add", addition)
             , ("add64", addition64)
             , ("ifStat", ifStatement)
             , ("gauss", gaussSum)
             , ("lt", lessThan)
             , ("rec", infiniteRecursion)
             , ("even", evenRec)
             , ("odd", oddRec)
             ]

runMdl = runFunc generalMdl
runMdlRD = runFuncRD generalMdl
runMdlIA = runFuncIA generalMdl
