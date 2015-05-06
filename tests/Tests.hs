module Main where

import Data.Char
import Ast
import TypeTree
import Parser
import JuniorTypeChecker
import JuniorParser
import Test.Hspec
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader

runParser :: Parser a -> PString -> [(a, PString)]
runParser m (p, s) = runStateT (runReaderT m p) (p,s)

main :: IO ()
main = hspec $ do

  describe "item tests" $ do
    it "should consume one char and move the position by 1" $ do
      runParser item ((0,0), "Hello World") `shouldBe` [('H',((0,1),"ello World"))]

    it "should fail if no character to consume" $ do
      runParser item ((0,0), "") `shouldBe` []

  describe "sat tests" $ do
    it "should consume only the chars that satisfy the predicate" $ do
      runParser (sat isUpper) ((0,0), "hello") `shouldBe` []
      runParser (sat isUpper) ((0,0), "Hello") `shouldBe` [('H', ((0, 1), "ello"))]

  describe "onside tests" $ do
    it "should be onside if on the same line or column greater than existing column" $ do
      onside (0,0) (0,0) `shouldBe` True
      onside (4,6) (4,2) `shouldBe` True
      onside (7,7) (8,5) `shouldBe` True
      onside (6,2) (5,6) `shouldBe` False
      onside (5,1) (3,4) `shouldBe` False

  describe "literal tests" $ do

    it "should parse integer literal" $ do
      runParser lit ((0,0),"42")
        `shouldBe` [(Literal (Int 42), ((0,2), ""))]

    it "should parse string literal" $ do
      runParser lit ((0,0), "\"Hello World\"")
        `shouldBe` [(Literal (String "Hello World"), ((0,13), ""))]

    it "should parse char literal" $ do
      runParser lit ((0,0), "'a'")
        `shouldBe` [(Literal (Char 'a'), ((0,3), ""))]

    it "should parse float literal" $ do
      runParser lit ((0,0), "13.64")
        `shouldBe` [(Literal (Float 13.64), ((0,5), ""))]

  describe "arithmetic tests" $ do

    it "should parse addition" $ do
      runParser expr ((0,0), "x + y")
        `shouldBe` [(App (App (Var "+") (Var "x")) (Var "y"), ((0,5), ""))]

    it "should parse addition with multiplication" $ do
      runParser expr ((0,0), "x + y * z")
        `shouldBe` [(App (App (Var "+") (Var "x")) (App (App (Var "*") (Var "y")) (Var "z")), ((0,9), ""))]

    it "should parse addition with multiplication with parens" $ do
      runParser expr ((0,0), "(x + y) * z")
        `shouldBe` [(App (App (Var "*") (App (App (Var "+") (Var "x")) (Var "y"))) (Var "z"),((0,11),""))]

  describe "boolean expressions tests" $ do

    it "should parse greater than expression" $ do
      runParser expr ((0,0), "x > y")
        `shouldBe` [(App (App (Var ">") (Var "x")) (Var "y"), ((0,5), ""))]

    it "should parse bool expression combined with arithmetic expression" $ do
      runParser expr ((0,0), "x + y > z ")
        `shouldBe` [(App (App (Var ">") (App (App (Var "+") (Var "x")) (Var "y"))) (Var "z"), ((0,10), ""))]

    it "should parse bool expression combined with arithmetic expression 2" $ do
      runParser expr ((0,0), "x > y + z ")
        `shouldBe` [(App (App (Var ">") (Var "x")) (App (App (Var "+") (Var "y")) (Var "z")), ((0,10), ""))]

  describe "lambda expression tests" $ do

    it "should parse lambda expression for identity function" $ do
      runParser lam ((0,0), "\\x -> x")
        `shouldBe` [(Lam "x" (Var "x"), ((0,7), ""))]

    it "should parse a lambda of a lambda containing an arithmetic expression" $ do
      runParser lam ((0,0), "\\x -> \\y -> x + y")
        `shouldBe` [(Lam "x" (Lam "y" (App (App (Var "+") (Var "x")) (Var "y"))), ((0,17), ""))]

  describe "Let expression tests" $ do

    it "shoulbe parse simple let expression" $ do
      runParser expr ((0,0), "let x = v in x")
        `shouldBe` [(Let [("x",Var "v")] (Var "x"),((0,14),""))]

    it "should parse simple let expression with carriage return using offside rule" $ do
      runParser expr ((0,0), "let x = v\n in x")
         `shouldBe` [(Let [("x",Var "v")] (Var "x"),((1,5),""))]

      runParser expr ((0,0), "let x = v\n    y = w\n in x")
         `shouldBe` [(Let [("x",Var "v"),("y",Var "w")] (Var "x"),((2,5),""))]

    it "should parse let expressions containing literals" $ do
      runParser expr ((0,0), "let x = 42 in x")
         `shouldBe` [(Let [("x",Literal (Int 42))] (Var "x"),((0,15),""))]

    it "should parse let expressions containing literals and arithmetic expressions" $ do
      runParser expr ((0,0), "let x = 5 in x + x")
         `shouldBe` [(Let [("x",Literal (Int 5))] (App (App (Var "+") (Var "x")) (Var "x")),((0,18),""))]

      runParser expr ((0,0), "let x = 5 in (x + 1) * y")
         `shouldBe` [(Let [("x",Literal (Int 5))] (App (App (Var "*") (App (App (Var "+") (Var "x")) (Literal (Int 1)))) (Var "y")),((0,24),""))]

      runParser expr ((0,0), "let x = y + 1 in x + 2")
         `shouldBe` [(Let [("x",App (App (Var "+") (Var "y")) (Literal (Int 1)))] (App (App (Var "+") (Var "x")) (Literal (Int 2))),((0,22),""))]

    it "should parse let expressions containing lambda expressions" $ do
      runParser expr ((0,0), "let f = \\x -> x + 1 in f 5")
         `shouldBe` [(Let [("f",Lam "x" (App (App (Var "+") (Var "x")) (Literal (Int 1))))] (App (Var "f") (Literal (Int 5))),((0,26),""))]

  describe "Global declaration tests" $ do

    it "should parse a top level simple declaration" $ do
      runParser globalDef ((0,0), "x = 42")
        `shouldBe` [([DeclValue "x" (Literal (Int 42))], ((0, 6), ""))]

    it "should parse a top level declaration with an arithmetic expression inside" $ do
      runParser globalDef ((0,0), "x = 3 + y")
        `shouldBe` [([DeclValue "x" (App (App (Var "+") (Literal (Int 3))) (Var "y"))], ((0,9), ""))]

    it "should parse a top level decl with a lambda inside" $ do
      runParser globalDef ((0,0), "f = \\x -> x")
        `shouldBe` [([DeclValue "f" (Lam "x" (Var "x"))], ((0,11), ""))]

    it "should parse a top level decl with a lambda containing an arithmetic expression" $ do
      runParser globalDef ((0,0), "f = \\x -> x + 1")
        `shouldBe` [([DeclValue "f" (Lam "x" (App (App (Var "+") (Var "x")) (Literal (Int 1))))], ((0,15), ""))]

    it "should parse multiple top level declarations" $ do
      runParser globalDef ((0,0), "x = 42\ny = 32")
        `shouldBe` [([DeclValue "x" (Literal (Int 42)), DeclValue "y" (Literal (Int 32))], ((1,6), ""))]

    it "should parse a declaration with an embeded let expression" $ do
      runParser globalDef ((0,0), "x = let y = 42 in y")
        `shouldBe` [([DeclValue "x" (Let [("y", Literal (Int 42))] (Var "y"))], ((0,19), ""))]

  describe "Type Checker tests" $ do

    it "should type check literals" $ do
      typeOf (Literal (Int 42))
        `shouldBe` (TyCon("int", []))

    it "should type check identity expression" $ do
      typeOf (Lam "x" (Var "x"))
        `shouldBe` (TyLam (TyVar "a") (TyVar "a"))

    it "should type check increment expression" $ do
      typeOf (Lam "x" (App (App (Var "+") (Var "x")) (Literal (Int 1))))
        `shouldBe` (TyLam (TyCon("int", [])) (TyCon("int", [])))

    it "should type check simple let expression" $ do
      typeOf (Let [("x",Literal (Int 42))] (Var "x"))
        `shouldBe` (TyCon("int", []))

    it "should type check let expression with two bindings" $ do
      typeOf (Let [("x",Literal (Int 42)), ("y", Literal (Int 24))] (App (App (Var "+") (Var "x")) (Var "y")))
        `shouldBe` (TyCon("int", []))

    it "should type check top level decl" $ do
      typeOfProg (Prog [(DeclValue "x" (Literal (Int 42)))])
        `shouldBe` [("x", TyCon("int", []))]

    it "should type check top level two indipendent decls" $ do
      typeOfProg (Prog [DeclValue "x" (Literal (Int 42)), DeclValue "y" (Literal (String "Hello"))])
        `shouldBe` [("y",TyCon ("string",[])),("x",TyCon ("int",[]))]

    it "should type check top level with two decls" $ do
      typeOfProg (Prog [DeclValue "x" (Literal (Int 42)), DeclValue "y" (Var "x")])
        `shouldBe` [("y",TyCon ("int",[])),("x",TyCon ("int",[]))]









