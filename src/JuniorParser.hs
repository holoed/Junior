{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}

module JuniorParser where

import Parser

data Lit = Int Int
         | String String
         deriving (Show, Eq)

data Expr = Decl [(String, Expr)]
          | App Expr Expr
          | Lam String Expr
          | Let [(String, Expr)] Expr
          | Literal Lit
          | Var String
          deriving (Show, Eq)


infixOp :: String-> Expr -> Expr -> Expr
infixOp s x y =  App (App (Var s) x) y

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (do { symbol "+"; return (infixOp "+") }) <||>
        (do { symbol "-"; return (infixOp "-") })

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = (do { symbol "*"; return (infixOp "*") }) <||>
        (do { symbol "/"; return (infixOp "/") })

arith_expr :: Parser Expr
arith_expr = term `chainl1` addOp

term :: Parser Expr
term = atom `chainl1` mulOp

expr :: Parser Expr
expr = arith_expr `chainl1` (return App)

globalDef :: Parser Expr
globalDef = do ds <- many1_offside defn
               return (Decl ds)

atom :: Parser Expr
atom = lam <||> local <||> var <||> lit <||> paren

lam :: Parser Expr
lam = do symbol "\\"
         x <- variable
         symbol "->"
         e <- expr
         return (Lam x e)

local :: Parser Expr
local = do symbol "let"
           ds <- many1_offside defn
           symbol "in"
           e <- expr
           return (Let ds e)

defn :: Parser (String, Expr)
defn = do x <- variable
          symbol "="
          e <- expr
          return (x, e)

var :: Parser Expr
var = fmap Var variable

lit :: Parser Expr
lit = fmap Literal literal

quoted_string :: Parser String
quoted_string = do symbol "\""
                   s <- many' (sat (\x -> x /= '\"'))
                   symbol "\""
                   return s

literal :: Parser Lit
literal = fmap Int (token int) <||>
          fmap String (token quoted_string)

paren :: Parser Expr
paren = brackets (symbol "(") expr (symbol ")")

variable :: Parser String
variable = identifier ["let", "in"]




              