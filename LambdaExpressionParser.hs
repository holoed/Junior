module LambdaExpressionParser where

import Parser
import Control.Monad.Trans.State.Lazy

data Expr = App Expr Expr
          | Lam String Expr
          | Let String Expr Expr
          | Var String deriving Show

expr :: Parser Expr
expr = atom `chainl1` (return App)

atom :: Parser Expr
atom = lam <||> local <||> var <||> paren

lam :: Parser Expr
lam = do symbol "\\"
         x <- variable
         symbol "->"
         e <- expr
         return (Lam x e)

local :: Parser Expr
local = do symbol "let"
           x <- variable
           symbol "="
           e <- expr
           symbol "in"
           e' <- expr
           return (Let x e e')

var :: Parser Expr
var = fmap Var variable

paren :: Parser Expr
paren = brackets (symbol "(") expr (symbol ")")

variable :: Parser String
variable = identifier ["let", "in"]


              