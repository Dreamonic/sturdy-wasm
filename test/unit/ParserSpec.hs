module ParserSpec (spec) where

    import Test.QuickCheck
    import Test.Hspec
    import Parser
    import Lexer

    spec :: Spec
    spec = do
        describe "parse function" $ do
            it "Should be able to parse a simple add function" $
                parse Parser.function [LP,Keyword "func",ID "add",LP,Keyword "param",ID "lhs",Keyword "i32",RP,LP,Keyword "param",ID "rhs",Keyword "i32",RP,LP,Keyword "result",Keyword "i32",RP,Keyword "get_local",ID "lhs",Keyword "get_local",ID "rhs",Keyword "i32.add",RP] `shouldBe`
                [(Func "add" [Param "lhs" I32,Param "rhs" I32] (Block [Result I32] [LocalGet "lhs",LocalGet "rhs",Numeric (Add I32)]),[])]