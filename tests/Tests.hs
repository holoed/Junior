module Main where

import Base
import Parser (parse)
import Interpreter (eval)
import Test.Hspec

run :: String -> IO Result
run = eval . parse

main :: IO ()
main = do txt <- readFile "jnr/Code.jnr"
          hspec $ do
            describe "fake tests" $ do
              it "what is the answer to life the universe and everything" $ do
                print (parse txt)
                ret <- run txt
                print ret
                42 `shouldBe` 42
