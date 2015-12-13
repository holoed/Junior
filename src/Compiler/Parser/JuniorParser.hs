{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}

module Compiler.Parser.JuniorParser where

import Compiler.Ast
import Compiler.Parser.BaseParsers

infixOp :: String-> Expr -> Expr -> Expr
infixOp s x =  App (App (Var s) x)

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
expr = foldl chainl1 atom [mulOp, addOp, cmpOp, return App]

globalDef :: Parser [Decl]
globalDef = do ds <- many1_offside defn
               return (map (uncurry DeclValue) ds)

atom :: Parser Expr
atom = ifThenElse <||> lam <||> local <||> var <||> lit <||> paren

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

quotedString :: Parser String
quotedString = do symbol "\""
                  s <- many' (sat (/= '\"'))
                  symbol "\""
                  return s

quotedChar :: Parser Char
quotedChar = do symbol "'"
                c <- sat (/= '\'')
                symbol "'"
                return c

bool :: Parser Bool
bool = do { symbol "True"; return True } <||>
       do { symbol "False"; return False }

literal :: Parser Lit
literal = fmap Float (token float) <||>
          fmap Int (token int) <||>
          fmap String (token quotedString) <||>
          fmap Char (token  quotedChar) <||>
          fmap Bool (token bool)

paren :: Parser Expr
paren = brackets (symbol "(") expr (symbol ")")

variable :: Parser String
variable = identifier ["let", "in", "if", "then", "else"]
