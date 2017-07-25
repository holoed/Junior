module Main where

import Data.Char
import Data.Map hiding (null, map)
import Control.Monad.Reader hiding (fix)
import RecursionSchemes
import Ast
import Parser hiding (local)

type Env = Map String Result
type EnvReader a = Reader Env a
data Prim = IntVal Int
          | FloatVal Float
          | StringVal String
          | BoolVal Bool
          | UnitVal deriving (Show, Eq)

data Expr =  EVar String
           | ELit Prim
           | ELet String Expr Expr
           deriving Show

data Result = Value Prim
            | Function (Result -> EnvReader Result)
            | TupleResult [Result]
            | ListResult [Result]
            | ExpResult Expr

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

interpAlg :: ExpF (EnvReader Result) -> EnvReader Result
interpAlg (Lit (IntLit x)) = return (Value (IntVal x))
interpAlg (Lit (StringLit s)) = return (Value (StringVal s))
interpAlg (Lit UnitLit) = return (Value UnitVal)
interpAlg (Var s) = fmap (\env -> maybe (error ("missing value " ++ s)) id (Data.Map.lookup s env)) ask
interpAlg (Lam s b) = fmap (\env -> Function (\r -> local (\env2 -> insert s r (env `union` env2)) b)) ask
interpAlg (App e1 e2) = do { fn <- e1; arg <- e2; applyFn fn arg }
interpAlg (Let s v b) = do { v' <- v; local (insert s v') b }
interpAlg (IfThenElse p e1 e2) = do { Value (BoolVal p') <- p; if p' then e1 else e2 }

eq (IntVal x) (IntVal y) = x == y
eq (StringVal x) (StringVal y) = x == y

isEmpty :: Result -> EnvReader Result
isEmpty (ListResult xs) = return $ Value (BoolVal (null xs))
isEmpty (Value (StringVal xs)) = return $ Value (BoolVal (null xs))

hd :: Result -> EnvReader Result
hd (ListResult xs) = return $ head xs
hd (Value (StringVal xs)) = return $ Value $ StringVal [head xs]

tl :: Result -> EnvReader Result
tl (ListResult xs) = return $ ListResult (tail xs)
tl (Value (StringVal xs)) = return $ Value $ StringVal (tail xs)

add :: Prim -> Prim -> Prim
add (IntVal x) (IntVal y) = IntVal (x + y)
add (IntVal x) (FloatVal y) = FloatVal (fromIntegral x + y)

sub :: Prim -> Prim -> Prim
sub (IntVal x) (IntVal y) = IntVal (x - y)
sub (IntVal x) (FloatVal y) = FloatVal (fromIntegral x - y)

mul :: Prim -> Prim -> Prim
mul (IntVal x) (IntVal y) = IntVal (x * y)
mul (IntVal x) (FloatVal y) = FloatVal (fromIntegral x * y)

divz :: Prim -> Prim -> Prim
divz (IntVal x) (IntVal y) = FloatVal (fromIntegral x / fromIntegral y)

gt :: Prim -> Prim -> Prim
gt (IntVal x) (IntVal y) = BoolVal (x > y)

predefEnv :: Env
predefEnv = fromList [
    ("==", Function (\(Value x) -> return $ Function (\(Value y) -> return $ Value (BoolVal (eq x y)) ))),
    ("/=", Function (\(Value x) -> return $ Function (\(Value y) -> return $ Value (BoolVal (not (eq x y))) ))),
    ("-", Function (\x -> return $ Function (\y -> return $ Value (sub (valueOf x) (valueOf y)) ))),
    ("+", Function (\x -> return $ Function (\y -> return $ Value (add (valueOf x) (valueOf y)) ))),
    ("*", Function (\x -> return $ Function (\y -> return $ Value (mul (valueOf x) (valueOf y)) ))),
    ("/", Function (\x -> return $ Function (\y -> return $ Value (divz (valueOf x) (valueOf y)) ))),
    (">", Function (\x -> return $ Function (\y -> return $ Value (gt (valueOf x) (valueOf y)) ))),
    ("||", Function (\(Value (BoolVal a)) -> return $ Function (\(Value (BoolVal b)) -> return $ Value (BoolVal (a || b)) ))),
    (":", Function (\x -> return $ Function (\(ListResult xs) -> return $ ListResult (x : xs)))),
    ("isEmpty", Function isEmpty),
    ("head", Function hd),
    ("tail", Function tl),
    ("empty", ListResult []),
    ("fst", Function (\(TupleResult xs) -> return $ head xs)),
    ("snd", Function (\(TupleResult xs) -> return $ head (tail xs))),
    ("neg", Function (\(Value (IntVal n)) -> return $ Value (IntVal (-n)))),
    ("mkTuple2", Function (\x -> return $ Function (\y -> return $ TupleResult [x, y]))),
    ("isLower", Function (\(Value (StringVal s)) -> return $ Value (BoolVal (isLower (head s))))),
    ("isUpper", Function (\(Value (StringVal s)) -> return $ Value (BoolVal (isUpper (head s))))),
    ("stringToCharList", Function (\(Value (StringVal s)) -> return $ ListResult (fmap (\x -> Value $ StringVal [x]) s))),
    ("charListToString", Function (\(ListResult xs) -> return $ Value (StringVal (concatMap (\(Value (StringVal x)) -> x) xs)))),
    ("stringToInt", Function (\(Value (StringVal s)) -> return $ Value (IntVal (read s)))),
    ("size", Function (\(Value (IntVal n)) -> return $ Value (IntVal (10 ^ length (show n))))),
    ("notElem", Function (\x -> return $ Function (\(ListResult xs) -> return $ Value (BoolVal (notElem x xs))))),
    ("evar", Function (\(Value (StringVal s)) -> return $ ExpResult (EVar s))),
    ("elit", Function (\(Value x) -> return $ ExpResult (ELit x))),
    ("elet", Function (\(Value (StringVal s)) -> return $ Function (\(ExpResult e1) -> return $ Function (\(ExpResult e2) -> return $ ExpResult (ELet s e1 e2)))))
  ]

eval :: Exp -> Result
eval e = runReader (cataRec interpAlg e) predefEnv

run :: String -> Result
run = eval . parse

printResult :: Result -> String
printResult (Value (StringVal x)) = show x
printResult (Value x) = show x
printResult (ListResult xs) =  show (fmap printResult xs)
printResult (TupleResult [x, y]) = show (printResult x, printResult y)
printResult (ExpResult e) = show e
printResult (Function _) = "<fun>"

main :: IO ()
main = do
  txt <- readFile "Code.foo"
  putStrLn $ printResult $ run txt
