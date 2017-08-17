module Main where

import Data.Map hiding (null, map)
import Control.Monad.Reader hiding (fix)
import Experiments.RecursionSchemes
import Experiments.Ast
import Experiments.Parser hiding (local)
import Primitives
import Base
import Lib

interpAlg :: ExpF (EnvReader Result) -> EnvReader Result
interpAlg (Lit (IntLit x)) = return (Value (IntVal x))
interpAlg (Lit (StringLit s)) = return (Value (StringVal s))
interpAlg (Lit UnitLit) = return (Value UnitVal)
interpAlg (Var s) = fmap (\env -> maybe (error ("missing value " ++ s)) id (Data.Map.lookup s env)) ask
interpAlg (Lam s b) = fmap (\env -> Function (\r -> local (\env2 -> insert s r (env `union` env2)) b)) ask
interpAlg (App e1 e2) = do { fn <- e1; arg <- e2; applyFn fn arg }
interpAlg (Let s v b) = do { v' <- v; local (insert s v') b }
interpAlg (IfThenElse p e1 e2) = do { Value (BoolVal p') <- p; if p' then e1 else e2 }

eval :: Exp -> IO Result
eval e = runReaderT (cataRec interpAlg e) predefEnv

run :: String -> IO Result
run = eval . parse

main :: IO ()
main = do
    --  tst <- readFile "testString.jnr"
    --  print tst
    --  print (parse tst)
  ext <- readFile "Externals.js"
  txt <- readFile "Code.jnr"
  print (parse txt)
  ret <- run txt
  print ret
  writeFile "bin.js" $ ext ++ (printResult $ ret)
