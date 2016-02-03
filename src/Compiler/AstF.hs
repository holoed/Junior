{-# LANGUAGE DeriveFunctor #-}
module Compiler.AstF where

import Compiler.Rec
import qualified Compiler.Ast as Ast

data ExpF a = App a a
            | Lam String a
            | Let [(String, a)] a
            | Literal Ast.Lit
            | Var String
            | IfThenElse a a a
     deriving (Show, Eq, Functor)

app :: Fix ExpF -> Fix ExpF -> Fix ExpF
app e1 e2 = In (App e1 e2)

lam :: String -> Fix ExpF -> Fix ExpF
lam s e = In (Lam s e)

leT :: [(String, Fix ExpF)] -> Fix ExpF -> Fix ExpF
leT bs e = In (Let bs e)

lit :: Ast.Lit -> Fix ExpF
lit x = In (Literal x)

var :: String -> Fix ExpF
var x = In (Var x)

iF :: Fix ExpF -> Fix ExpF -> Fix ExpF -> Fix ExpF
iF p e1 e2 = In (IfThenElse p e1 e2)

toExpF :: Ast.Expr -> Fix ExpF
toExpF (Ast.App e1 e2) = app (toExpF e1) (toExpF e2)
toExpF (Ast.Lam s e) = lam s (toExpF e)
toExpF (Ast.Let bs e) = leT (map (\(s, e') -> (s, toExpF e')) bs) (toExpF e)
toExpF (Ast.Literal x) = lit x
toExpF (Ast.Var x) = var x
toExpF (Ast.IfThenElse p e1 e2) = iF (toExpF p) (toExpF e1) (toExpF e2)

toExp :: Fix ExpF -> Ast.Expr
toExp = cata f 
  where
    f :: ExpF Ast.Expr -> Ast.Expr
    f (App e1 e2) = Ast.App e1 e2
    f (Lam s e') = Ast.Lam s e'
    f (Let bs e') = Ast.Let bs e'
    f (Literal x) = Ast.Literal x
    f (Var x) = Ast.Var x
    f (IfThenElse p e1 e2) = Ast.IfThenElse p e1 e2
