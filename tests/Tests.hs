module Main where

import Base
import Primitives
import Parser (parse)
import Interpreter (eval)
import Test.Hspec

run :: String -> IO Result
run = eval . parse

main :: IO ()
main = do ext <- readFile "js/Externals.js"
          codeTxt <- readFile "jnr/Code.jnr"
          expected <- readFile "js/CodeV2.js"
          hspec $ do
            describe "Junior Interpreter Tests" $ do
              it "Should parse and interpret Code.jnr and produce the right javascript" $ do
                print (parse codeTxt)
                Value (StringVal actual) <- run codeTxt
                print actual
                (ext ++ actual) `shouldBe` expected
