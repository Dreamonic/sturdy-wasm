module Interp.SharedHO.Programs
where

import Interp.SharedHO.ToyInterpreter

addition :: Expr
addition = Seq [Const 2, Const 5, Add]

assigns :: Expr
assigns = Seq [Const 0, Assign "x",
               Const 1, Assign "y",
               Const 2, Var "x", Add, Assign "x",
               Var "x", Var "y", Add, Assign "y",
               Var "y"]

ifStatement :: Expr
ifStatement = Seq [Const 1, Assign "y",
                   Const 0, If
                       (Seq [Const 2, Assign "y"])
                       (Seq [Const 3, Assign "y"]),
                   Var "y"]

finiteLoop :: Expr
finiteLoop = Seq [Const 0, Assign "x",
                  Loop $ Seq [Var "x", Const 5, Lt, If
                            (Seq [Const 1, Var "x", Add, Assign "x",
                                  Branch 0])
                            (Var "x")]]

infiniteLoop :: Expr
infiniteLoop = Seq [Const 0, Assign "x",
                    Loop $ Seq [Const 1, Var "x", Add, Assign "x",
                                Branch 0],
                    Var "x"]

undecidableIf :: Expr
undecidableIf = Seq [Const 0, Assign "x",
                     Loop $ Seq [Const 1, Var "x", Add, Assign "x",
                                 Branch 0],
                     Var "x", If
                         (Seq [Const 2, Assign "y"])
                         (Seq [Const 3, Assign "y"]),
                     Var "y"]

nestedBlock :: Expr
nestedBlock = Seq [Block $ Seq [Block $ Seq [Const 0,
                                             Branch 1,
                                             Const 1, Add],
                                Const 2, Add],
                   Const 4, Add]


nestedLoop :: Expr
nestedLoop = Seq [Const 0, Assign "x",
                  Loop $ Seq [Var "x", Const 5, Lt, If
                      (Seq [Const 1, Var "x", Add, Assign "x",
                            Const 0, Assign "y",
                            Loop $ Seq [Var "y", Const 5, Var "x", Add, Lt, If
                                (Seq [Const 1, Var "y", Add, Assign "y",
                                      Branch 0])
                                Nop],
                            Branch 0])
                      Nop]]
