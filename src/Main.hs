module Main where

import Base
import Parser (parse)
import Interpreter (eval)

run :: String -> IO Result
run = eval . parse

main :: IO ()
main = do
  ext <- readFile "js/Externals.js"
  txt <- readFile "jnr/Code.jnr"
  print (parse txt)
  ret <- run txt
  print ret
  writeFile "js/CodeV2.js" $ ext ++ (printResult $ ret)
