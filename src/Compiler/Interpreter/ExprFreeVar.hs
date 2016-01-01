module Compiler.Interpreter.ExprFreeVar where

import Compiler.Ast
import Data.List (delete)

freeVar :: Expr -> [String]
freeVar (Var x) = [x]
freeVar (Lam x e) = delete x (freeVar e)
freeVar (App e1 e2) = freeVar e1 ++ freeVar e2
freeVar (Literal _) = []
freeVar (IfThenElse b e1 e2) = freeVar b ++ freeVar e1 ++ freeVar e2
freeVar (Let bs e) = remove bs (freeVar e)
  where
    remove :: [(String, Expr)] -> [String] -> [String]
    remove ((x,e'):bs') s = remove bs' (delete x (freeVar e' ++ s))
    remove [] s = s
