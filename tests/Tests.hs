module Main where

import Data.Char
import Parser
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