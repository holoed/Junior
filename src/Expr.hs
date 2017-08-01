module Expr where

import Primitives

data Expr =  EVar String
           | ELit Prim
           | ELam String Expr
           | EApp Expr Expr
           | ELet String Expr Expr
           | EIfThenElse Expr Expr Expr
           deriving Show

isVar :: Expr -> Bool
isVar (EVar _) = True
isVar _ = False

isLit :: Expr -> Bool
isLit (ELit _) = True
isLit _ = False

isLet :: Expr -> Bool
isLet ELet {} = True
isLet _ = False

isApp :: Expr -> Bool
isApp EApp {} = True
isApp _ = False

isLam :: Expr -> Bool
isLam ELam {} = True
isLam _ = False

isIfThenElse :: Expr -> Bool
isIfThenElse EIfThenElse {} = True
isIfThenElse _ = False
