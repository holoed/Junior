module Compiler.Parser.JuniorParserTests where

import Data.Char
import Compiler.Ast
import Compiler.Parser.BaseParsers
import Compiler.Parser.JuniorParser
import Test.Hspec
import TestsUtils

parserTests :: Spec
parserTests = do
  describe "item tests" $ do
    it "should consume one char and move the position by 1" $
      runParser item ((0,0), "Hello World") `shouldBe` [('H',((0,1),"ello World"))]

    it "should fail if no character to consume" $
      runParser item ((0,0), "") `shouldBe` []

  describe "sat tests" $
    it "should consume only the chars that satisfy the predicate" $ do
      runParser (sat isUpper) ((0,0), "hello") `shouldBe` []
      runParser (sat isUpper) ((0,0), "Hello") `shouldBe` [('H', ((0, 1), "ello"))]

  describe "onside tests" $
    it "should be onside if on the same line or column greater than existing column" $ do
      onside (0,0) (0,0) `shouldBe` True
      onside (4,6) (4,2) `shouldBe` True
      onside (7,7) (8,5) `shouldBe` True
      onside (6,2) (5,6) `shouldBe` False
      onside (5,1) (3,4) `shouldBe` False

  describe "literal tests" $ do

    it "should parse integer literal" $
      runParser lit ((0,0),"42")
        `shouldBe` [(Literal (Int 42), ((0,2), ""))]

    it "should parse string literal" $
      runParser lit ((0,0), "\"Hello World\"")
        `shouldBe` [(Literal (String "Hello World"), ((0,13), ""))]

    it "should parse char literal" $
      runParser lit ((0,0), "'a'")
        `shouldBe` [(Literal (Char 'a'), ((0,3), ""))]

    it "should parse float literal" $
      runParser lit ((0,0), "13.64")
        `shouldBe` [(Literal (Float 13.64), ((0,5), ""))]

    it "should parse boolean literal" $
      runParser lit ((0,0), "True")
        `shouldBe` [(Literal (Bool True), ((0,4), ""))]

  describe "arithmetic tests" $ do

    it "should parse addition" $
      runParser expr ((0,0), "x + y")
        `shouldBe` [(App (App (Var "+") (Var "x")) (Var "y"), ((0,5), ""))]

    it "should parse addition with multiplication" $
      runParser expr ((0,0), "x + y * z")
        `shouldBe` [(App (App (Var "+") (Var "x")) (App (App (Var "*") (Var "y")) (Var "z")), ((0,9), ""))]

    it "should parse addition with multiplication with parens" $
      runParser expr ((0,0), "(x + y) * z")
        `shouldBe` [(App (App (Var "*") (App (App (Var "+") (Var "x")) (Var "y"))) (Var "z"),((0,11),""))]

  describe "boolean expressions tests" $ do

    it "should parse greater than expression" $
      runParser expr ((0,0), "x > y")
        `shouldBe` [(App (App (Var ">") (Var "x")) (Var "y"), ((0,5), ""))]

    it "should parse bool expression combined with arithmetic expression" $
      runParser expr ((0,0), "x + y > z ")
        `shouldBe` [(App (App (Var ">") (App (App (Var "+") (Var "x")) (Var "y"))) (Var "z"), ((0,10), ""))]

    it "should parse bool expression combined with arithmetic expression 2" $
      runParser expr ((0,0), "x > y + z ")
        `shouldBe` [(App (App (Var ">") (Var "x")) (App (App (Var "+") (Var "y")) (Var "z")), ((0,10), ""))]

  describe "should parse if expressions" $

    it "should parse simple if then else expression" $
      runParser ifThenElse ((0,0), "if True then 12 else 25")
        `shouldBe` [(IfThenElse (Literal (Bool True)) (Literal (Int 12)) (Literal (Int 25)), ((0,23), ""))]

  describe "lambda expression tests" $ do

    it "should parse lambda expression for identity function" $
      runParser lam ((0,0), "\\x -> x")
        `shouldBe` [(Lam "x" (Var "x"), ((0,7), ""))]

    it "should parse a lambda of a lambda containing an arithmetic expression" $
      runParser lam ((0,0), "\\x -> \\y -> x + y")
        `shouldBe` [(Lam "x" (Lam "y" (App (App (Var "+") (Var "x")) (Var "y"))), ((0,17), ""))]

  describe "Let expression tests" $ do

    it "shoulbe parse simple let expression" $
      runParser expr ((0,0), "let x = v in x")
        `shouldBe` [(Let [("x",Var "v")] (Var "x"),((0,14),""))]

    it "should parse simple let expression with carriage return using offside rule" $ do
      runParser expr ((0,0), "let x = v\n in x")
         `shouldBe` [(Let [("x",Var "v")] (Var "x"),((1,5),""))]

      runParser expr ((0,0), "let x = v\n    y = w\n in x")
         `shouldBe` [(Let [("x",Var "v"),("y",Var "w")] (Var "x"),((2,5),""))]

    it "should parse let expressions containing literals" $
      runParser expr ((0,0), "let x = 42 in x")
         `shouldBe` [(Let [("x",Literal (Int 42))] (Var "x"),((0,15),""))]

    it "should parse let expressions containing literals and arithmetic expressions" $ do
      runParser expr ((0,0), "let x = 5 in x + x")
         `shouldBe` [(Let [("x",Literal (Int 5))] (App (App (Var "+") (Var "x")) (Var "x")),((0,18),""))]

      runParser expr ((0,0), "let x = 5 in (x + 1) * y")
         `shouldBe` [(Let [("x",Literal (Int 5))] (App (App (Var "*") (App (App (Var "+") (Var "x")) (Literal (Int 1)))) (Var "y")),((0,24),""))]

      runParser expr ((0,0), "let x = y + 1 in x + 2")
         `shouldBe` [(Let [("x",App (App (Var "+") (Var "y")) (Literal (Int 1)))] (App (App (Var "+") (Var "x")) (Literal (Int 2))),((0,22),""))]

    it "should parse let expressions containing lambda expressions" $
      runParser expr ((0,0), "let f = \\x -> x + 1 in f 5")
         `shouldBe` [(Let [("f",Lam "x" (App (App (Var "+") (Var "x")) (Literal (Int 1))))] (App (Var "f") (Literal (Int 5))),((0,26),""))]

  describe "Global declaration tests" $ do

    it "should parse a top level simple declaration" $
      runParser globalDef ((0,0), "x = 42")
        `shouldBe` [([DeclValue "x" (Literal (Int 42))], ((0, 6), ""))]

    it "should parse a top level declaration with an arithmetic expression inside" $
      runParser globalDef ((0,0), "x = 3 + y")
        `shouldBe` [([DeclValue "x" (App (App (Var "+") (Literal (Int 3))) (Var "y"))], ((0,9), ""))]

    it "should parse a top level decl with a lambda inside" $
      runParser globalDef ((0,0), "f = \\x -> x")
        `shouldBe` [([DeclValue "f" (Lam "x" (Var "x"))], ((0,11), ""))]

    it "should parse a top level decl with a lambda containing an arithmetic expression" $
      runParser globalDef ((0,0), "f = \\x -> x + 1")
        `shouldBe` [([DeclValue "f" (Lam "x" (App (App (Var "+") (Var "x")) (Literal (Int 1))))], ((0,15), ""))]

    it "should parse multiple top level declarations" $
      runParser globalDef ((0,0), "x = 42\ny = 32")
        `shouldBe` [([DeclValue "x" (Literal (Int 42)), DeclValue "y" (Literal (Int 32))], ((1,6), ""))]

    it "should parse a declaration with an embeded let expression" $
      runParser globalDef ((0,0), "x = let y = 42 in y")
        `shouldBe` [([DeclValue "x" (Let [("y", Literal (Int 42))] (Var "y"))], ((0,19), ""))]
