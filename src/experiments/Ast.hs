{-# LANGUAGE DeriveFunctor #-}
module Ast where

import RecursionSchemes

data Literal = IntLit Int | StringLit String | UnitLit deriving Show

data ExpF a = Var String
            | App a a
            | Lam String a
            | Lit Literal
            | Let String a a
            | IfThenElse a a a
            deriving (Functor, Show)

type Exp = Fix ExpF

data Decl = Decl String Exp deriving Show

var :: String -> Exp
var x = In (Var x)

app :: Exp -> Exp -> Exp
app e1 e2 = In (App e1 e2)

lam :: String -> Exp -> Exp
lam s e = In (Lam s e)

lit :: Literal -> Exp
lit x = In (Lit x)

leT :: String -> Exp -> Exp -> Exp
leT s v b = In (Let s v b)

ifThenElse :: Exp -> Exp -> Exp -> Exp
ifThenElse p e1 e2 = In (IfThenElse p e1 e2)
