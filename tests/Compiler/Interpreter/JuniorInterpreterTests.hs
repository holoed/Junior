module Compiler.Interpreter.JuniorInterpreterTests where

import Compiler.Ast
import Compiler.TypeInference.TypeTree
import Compiler.Interpreter.JuniorInterpreter
import Test.Hspec
import TestsUtils

interpreterTests :: Spec
interpreterTests =

  describe "Interpreter tests" $ do
    it "should eval prog 1" $
      runProg "main = 42" `shouldBe` ([("main", TyCon("int", []))], [Const (Int 42)])

    it "should eval prog 2" $
      runProg "main = 2 + 3" `shouldBe` ([("main", TyCon("int", []))], [Const (Int 5)])

    it "should eval prog 3" $
      runProg "main = (2 + 3) * 5" `shouldBe` ([("main", TyCon("int", []))], [Const (Int 25)])

    it "should eval prog 4" $
      runProg "main = 2 + 3 * 5" `shouldBe` ([("main", TyCon("int", []))], [Const (Int 17)])

    it "should eval prog 5" $
      runProg ("x = 6\r\n" ++
               "y = 4\r\n" ++
               "main = (x + y) * (x - y)") `shouldBe`
               ([("main",TyCon ("int",[])),("y",TyCon ("int",[])),("x",TyCon ("int",[]))],[Const (Int 20)])

    it "should eval prog 6" $
       runProg ("x = 6\r\n" ++
                "y = x + 5\r\n" ++
                "main = (x + y) * 3") `shouldBe`
                ([("main",TyCon ("int",[])),("y",TyCon ("int",[])),("x",TyCon ("int",[]))],[Const (Int 51)])

    it "should eval prog 7" $
       runProg ("main = let x = 23\r\n" ++
                "           y = 12 in x + y") `shouldBe` ([("main", TyCon("int", []))], [Const (Int 35)])

    it "should eval prog 8" $ do
       runProg "main = if True then 12 else 25" `shouldBe` ([("main", TyCon("int", []))], [Const (Int 12)])
       runProg "main = if False then 12 else 25" `shouldBe` ([("main", TyCon("int", []))], [Const (Int 25)])

    it "should eval prog 9" $
       runProg ("f = \\n -> if (n == 0) then 12 else 25 \r\n" ++
                "main = f 5") `shouldBe`
                ([("main",TyCon ("int",[])),("f",TyLam (TyCon ("int",[])) (TyCon ("int",[])))], [Const (Int 25)])

    it "should eval prog 10" $
       runProg ("fac = \\n -> if (n == 0) then 1 else (n * (fac (n - 1))) \r\n" ++
                "main = fac 5") `shouldBe`
                ([("main",TyCon ("int",[])),("fac",TyLam (TyCon ("int",[])) (TyCon ("int",[])))], [Const (Int 120)])

    it "should eval prog 11" $
       runProg ("fib = \\n -> if (n == 0) then 0 else (if (n == 1) then 1 else (if (n == 2) then 1 else ((fib (n - 1)) + (fib (n - 2))))) \r\n" ++
                "main = fib 10") `shouldBe`
                ([("main",TyCon ("int",[])),("fib",TyLam (TyCon ("int",[])) (TyCon ("int",[])))], [Const (Int 55)])
