module Ast where

data Lit = Int Int
         | String String
         | Float Float
         | Char Char
     deriving (Show, Eq)

data Expr = App Expr Expr
          | Lam String Expr
          | Let [(String, Expr)] Expr
          | Literal Lit
          | Var String
     deriving (Show, Eq)

data Decl = DeclValue String Expr
     deriving (Show, Eq)