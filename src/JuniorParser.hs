{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}

module JuniorParser where

import Ast
import Parser

infixOp :: String-> Expr -> Expr -> Expr
infixOp s x y =  App (App (Var s) x) y

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (do { symbol "+"; return (infixOp "+") }) <||>
        (do { symbol "-"; return (infixOp "-") })

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = (do { symbol "*"; return (infixOp "*") }) <||>
        (do { symbol "/"; return (infixOp "/") })

cmpOp :: Parser (Expr -> Expr -> Expr)
cmpOp = (do { symbol ">"; return (infixOp ">") }) <||>
        (do { symbol "<"; return (infixOp "<") }) <||>
        (do { symbol "=="; return (infixOp "==")})

expr :: Parser Expr
expr = foldl chainl1 atom [mulOp, addOp, cmpOp, (return App)]

globalDef :: Parser [Decl]
globalDef = do ds <- many1_offside defn
               return (map (\(name, e) -> DeclValue name e) ds)

atom :: Parser Expr
atom = lam <||> local <||> var <||> lit <||> paren

ifThenElse :: Parser Expr
ifThenElse = do symbol "if"
                e1 <- expr
                symbol "then"
                e2 <- expr
                symbol "else"
                e3 <- expr
                return (IfThenElse e1 e2 e3)

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
                   s <- many' (sat (/= '\"'))
                   symbol "\""
                   return s

quoted_char :: Parser Char
quoted_char = do symbol "'"
                 c <- sat (/= '\'')
                 symbol "'"
                 return c

bool :: Parser Bool
bool = do { symbol "True"; return True } <||>
       do { symbol "False"; return False }

literal :: Parser Lit
literal = fmap Float (token float) <||>
          fmap Int (token int) <||>
          fmap String (token quoted_string) <||>
          fmap Char (token  quoted_char) <||>
          fmap Bool (token bool)

paren :: Parser Expr
paren = brackets (symbol "(") expr (symbol ")")

variable :: Parser String
variable = identifier ["let", "in", "if", "then", "else"]




              