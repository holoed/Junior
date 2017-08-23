module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "fake tests" $ do
    it "what is the answer to life the universe and everything" $ do
      42 `shouldBe` 42
