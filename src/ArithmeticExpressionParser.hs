{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}

module ArithmeticExpressionParser where

import Parser

addOp :: Num a => Parser (a -> a -> a)
addOp = (do { symbol "+"; return (+) }) <||>
        (do { symbol "-"; return (-) })

mulOp :: Integral a => Parser (a -> a -> a)
mulOp = (do { symbol "*"; return (*) }) <||>
        (do { symbol "/"; return (div) })

expr :: Parser Int
expr = term `chainl1` addOp

term :: Parser Int
term = factor `chainl1` mulOp

factor :: Parser Int
factor = (token int) <||> (brackets (symbol "(") expr (symbol ")"))