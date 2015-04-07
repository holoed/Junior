module Main where

import Data.Char
import Parser
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














