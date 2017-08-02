module Base where

import Primitives
import Data.Map hiding (null, map)
import Control.Monad.Reader hiding (fix)
import Expr

type Env = Map String Result
type EnvReader a = ReaderT Env IO a 

data Result = Value Prim
            | Function (Result -> EnvReader Result)
            | TupleResult [Result]
            | ListResult [Result]
            | ExpResult Expr

instance Show Result where
  show (Value x) = show x
  show (TupleResult xs)  = show xs
  show (ListResult xs) = show xs
  show (ExpResult e) = show e

instance Eq Result where
  (==) (Value x) (Value y) = x == y
  (==) (TupleResult xs) (TupleResult ys) = error "Unexpected tuple"
  (==) (ListResult xs) (ListResult ys) = error "Unexpected list"

valueOf :: Result -> Prim
valueOf (Value x) = x
valueOf (ListResult _) = error "Unexpected List"
valueOf (TupleResult _) = error "Unexpected Tuple"
valueOf _ = error "Unexpected Function"

applyFn :: Result -> Result -> EnvReader Result
applyFn (Function f) = f
applyFn (Value x) = error ("Expected function but got " ++ show x)

printResult :: Result -> String
printResult (Value (StringVal x)) = show x
printResult (Value x) = show x
printResult (ListResult xs) =  show (fmap printResult xs)
printResult (TupleResult [x, y]) = show (printResult x, printResult y)
printResult (ExpResult e) = show e
printResult (Function _) = "<fun>"
