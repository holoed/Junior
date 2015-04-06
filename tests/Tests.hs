module Main where

import Data.Char
import Parser
import LambdaExpressionParser
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

  describe "Lambda expression tests" $ do
    it "shoulbe parse let expression" $ do
      runParser expr ((0,0), "let x = v in x") `shouldBe` [(Let [("x",Var "v")] (Var "x"),((0,14),""))]
      runParser expr ((0,0), "let x = v\n in x") `shouldBe` [(Let [("x",Var "v")] (Var "x"),((1,5),""))]
      runParser expr ((0,0), "let x = v\n    y = w\n in x") `shouldBe` [(Let [("x",Var "v"),("y",Var "w")] (Var "x"),((2,5),""))]




