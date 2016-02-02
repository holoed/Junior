module Main where

import Test.Hspec
import Compiler.Parser.JuniorParserTests
import Compiler.TypeInference.TypeCheckerTests
import Compiler.Interpreter.JuniorInterpreterTests

main :: IO ()
main = hspec $ do
  parserTests
  typeCheckerTests
  interpreterTests
