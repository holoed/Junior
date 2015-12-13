module Compiler.Ast where

data Lit = Int Int
         | String String
         | Float Float
         | Char Char
         | Bool Bool
     deriving (Show, Eq)

data Expr = App Expr Expr
          | Lam String Expr
          | Let [(String, Expr)] Expr
          | Literal Lit
          | Var String
          | IfThenElse Expr Expr Expr
     deriving (Show, Eq)

data Decl = DeclValue String Expr
     deriving (Show, Eq)

data Prog = Prog [Decl]
