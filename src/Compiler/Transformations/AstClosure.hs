module Compiler.Transformations.AstClosure where

import Compiler.Ast

data ExprC = App ExprC ExprC
          | Lam String ExprC
          | Let [(String, ExprC)] ExprC
          | Literal Lit
          | Var String
          | IfThenElse ExprC ExprC ExprC
          | MkClosure ExprC ExprC
          | MkEnvironment [(String, ExprC)]
          | ApplyClosure ExprC ExprC
          | LambdaConverted String ExprC
     deriving (Show, Eq)

data DeclC = DeclValue String ExprC
     deriving (Show, Eq)
