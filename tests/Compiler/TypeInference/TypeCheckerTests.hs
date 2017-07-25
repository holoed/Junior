module Compiler.TypeInference.TypeCheckerTests where

import Compiler.Ast
import Compiler.Parser.JuniorParser
import Compiler.TypeInference.TypeTree
import Compiler.TypeInference.TypeChecker
import Test.Hspec
import Control.Exception
import TestsUtils

typeCheckerTests :: Spec
typeCheckerTests = do
  describe "Type Checker tests" $ do

    it "should type check literals" $
      typeOf (Literal (Int 42))
        `shouldBe` TyCon("int", [])

    it "should type check identity expression" $
      typeOf (Lam "x" (Var "x"))
        `shouldBe` TyLam (TyVar "a") (TyVar "a")

    it "should type check increment expression" $
      typeOf (Lam "x" (App (App (Var "+") (Var "x")) (Literal (Int 1))))
        `shouldBe` TyLam (TyCon("int", [])) (TyCon("int", []))

    it "should type check greater than expression" $
      typeOf (Lam "x" (App (App (Var ">") (Var "x")) (Literal (Int 1))))
        `shouldBe` TyLam (TyCon("int", [])) (TyCon("bool", []))

    it "should type check less than expression" $
      typeOf (Lam "x" (App (App (Var "<") (Var "x")) (Literal (Int 1))))
        `shouldBe` TyLam (TyCon("int", [])) (TyCon("bool", []))

    it "should type check simple let expression" $
      typeOf (Let [("x",Literal (Int 42))] (Var "x"))
        `shouldBe` TyCon("int", [])

    it "should type check let expression with two bindings" $
      typeOf (Let [("x",Literal (Int 42)), ("y", Literal (Int 24))] (App (App (Var "+") (Var "x")) (Var "y")))
        `shouldBe` TyCon("int", [])

    it "should type check top level decl" $
      typeOfProg (Prog [DeclValue "x" (Literal (Int 42))])
        `shouldBe` [("x", TyCon("int", []))]

    it "should type check top level two indipendent decls" $
      typeOfProg (Prog [DeclValue "x" (Literal (Int 42)), DeclValue "y" (Literal (String "Hello"))])
        `shouldBe` [("y",TyCon ("string",[])),("x",TyCon ("int",[]))]

    it "should type check top level with two decls" $
      typeOfProg (Prog [DeclValue "x" (Literal (Int 42)), DeclValue "y" (Var "x")])
        `shouldBe` [("y",TyCon ("int",[])),("x",TyCon ("int",[]))]

    it "should type check ifThenElse" $ do
      evaluate(typeOf (IfThenElse (Literal (Int 42)) (Literal (Int 12)) (Literal (Int 25))))
        `shouldThrow` errorCall "Unification error of TyCon (\"int\",[]) and TyCon (\"bool\",[])"
      evaluate(typeOf (IfThenElse (Literal (Bool True)) (Literal (Bool False)) (Literal (Int 25))))
        `shouldThrow` errorCall "Unification error of TyCon (\"int\",[]) and TyCon (\"bool\",[])"
      evaluate(typeOf (IfThenElse (Literal (Bool True)) (Literal (Int 13)) (Literal (Bool True))))
        `shouldThrow` errorCall "Unification error of TyCon (\"bool\",[]) and TyCon (\"int\",[])"
      typeOf (IfThenElse (Literal (Bool True)) (Literal (Int 42)) (Literal (Int 25)))
        `shouldBe` TyCon("int", [])

  describe "Parse and Type Check tests" $ do

    it "should type check top level decl" $
      typeCheck "x = 42"
        `shouldBe` [("x",TyCon ("int",[]))]

    it "should type check more than one top level decl" $
      typeCheck ("x = 42\r\n" ++
                 "y = 24")
        `shouldBe` [("y",TyCon ("int",[])),("x",TyCon ("int",[]))]

    it "should type check nested expressions" $
      let code = "x = let y = 42\r\n" ++
                 "        in y + 1" in do
      runParser globalDef ((0,0), code)
        `shouldBe` [([DeclValue "x" (Let [("y",Literal (Int 42))] (App (App (Var "+") (Var "y")) (Literal (Int 1))))],((1,16),""))]
      typeCheck code
        `shouldBe` [("x",TyCon ("int",[]))]

    it "should type check poly call" $
      let code = "ret = let f = \\x -> \\y -> x in (f 5 'a') + (f 6 7)" in do
      typeCheck code
        `shouldBe` [("ret",TyCon ("int",[]))]
