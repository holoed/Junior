{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}

module Experiments.Parser where

import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader hiding (local)
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Data.Maybe
import qualified Experiments.Ast as Ast
import Experiments.BaseParsers

infixOp :: String-> Ast.Exp -> Ast.Exp -> Ast.Exp
infixOp s x =  Ast.app (Ast.app (Ast.var s) x)

addOp :: Parser (Ast.Exp -> Ast.Exp -> Ast.Exp)
addOp = (do { symbol "+"; return (infixOp "+") }) <|>
        (do { symbol "-"; return (infixOp "-") })

mulOp :: Parser (Ast.Exp -> Ast.Exp -> Ast.Exp)
mulOp = (do { symbol "*"; return (infixOp "*") }) <|>
        (do { symbol "/"; return (infixOp "/") })

cmpOp :: Parser (Ast.Exp -> Ast.Exp -> Ast.Exp)
cmpOp = (do { symbol ">"; return (infixOp ">") }) <|>
        (do { symbol "<"; return (infixOp "<") }) <|>
        (do { symbol "=="; return (infixOp "==")}) <|>
        (do { symbol "/="; return (infixOp "/=")}) <|>
        (do { symbol "&&"; return (infixOp "&&")}) <|>
        (do { symbol "||"; return (infixOp "||")}) <|>
        (do { symbol ":"; return (infixOp ":")})

expr :: Parser Ast.Exp
expr = foldl chainl1 atom [mulOp, addOp, cmpOp, return Ast.app]

atom :: Parser Ast.Exp
atom = ifThenElse <|> lam <|> local <|> var <|> lit <|> paren

ifThenElse :: Parser Ast.Exp
ifThenElse = do symbol "if"
                e1 <- expr
                symbol "then"
                e2 <- expr
                symbol "else"
                e3 <- expr
                return (Ast.ifThenElse e1 e2 e3)

lam :: Parser Ast.Exp
lam = do symbol "\\"
         x <- variable
         symbol "->"
         e <- expr
         return (Ast.lam x e)

local :: Parser Ast.Exp
local = do symbol "let"
           x <- variable
           symbol "="
           v <- expr
           symbol "in"
           b <- expr
           return (Ast.leT x v b)


var :: Parser Ast.Exp
var = fmap Ast.var (variable <|> symbol "*")

lit :: Parser Ast.Exp
lit = fmap (Ast.lit . Ast.IntLit) (token int) <|>
      fmap (Ast.lit . Ast.StringLit) (token quotedString) <|>
      fmap (Ast.lit . const Ast.UnitLit) (symbol "()")

paren :: Parser Ast.Exp
paren = brackets (symbol "(") expr (symbol ")")

variable :: Parser String
variable = identifier ["let", "in", "if", "then", "else"]

runParser :: Parser a -> PString -> [(a, PString)]
runParser m (p, s) = maybeToList $ runIdentity (runMaybeT (runStateT (runReaderT m p) (p,s)))

parse :: String -> Ast.Exp
parse s = fst $ head $ runParser expr ((0,0), s)
