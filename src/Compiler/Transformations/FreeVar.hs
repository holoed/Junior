module Compiler.Transformations.FreeVar where

import Compiler.Ast
import Data.List (delete, union)

freeVarExpr :: Expr -> [String]
freeVarExpr (Var x) = [x]
freeVarExpr (Lam x e) = delete x (freeVarExpr e)
freeVarExpr (App e1 e2) = freeVarExpr e1 `union` freeVarExpr e2
freeVarExpr (Literal _) = []
freeVarExpr (IfThenElse b e1 e2) = freeVarExpr b `union` freeVarExpr e1 `union` freeVarExpr e2
freeVarExpr (Let bs e) = remove bs (freeVarExpr e)
  where
    remove :: [(String, Expr)] -> [String] -> [String]
    remove ((x,e'):bs') s = remove bs' (delete x (freeVarExpr e' `union` s))
    remove [] s = s

freeVarDecl :: Decl -> [String]
freeVarDecl (DeclValue x e) = delete x (freeVarExpr e)

freeVarProg :: Prog -> [String]
freeVarProg (Prog decls) = remove (reverse decls) []
  where
    remove :: [Decl] -> [String] -> [String]
    remove (DeclValue x e':ds') s = remove ds' (delete x (freeVarExpr e' `union` s))
    remove [] s = s
