{-# LANGUAGE InstanceSigs #-}

module Parser where

import Prelude hiding (seq)
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Lazy

type Parser a = StateT String [] a

item :: Parser Char
item = do s <- get
          case s of 
          	[] -> mzero
          	(x:xs) -> do put xs
          	             return x

first :: Parser a -> Parser a
first p = mapStateT (take 1) p

many' :: Parser a -> Parser [a]
many' = first . many

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q =  first (p <|> q)

seq :: Parser a -> Parser b -> Parser (a, b)
seq p q = do  x <- p
              y <- q
              return (x, y)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           guard (p x)
           return x

char :: Char -> Parser Char
char ch = sat (==ch)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = lower <||> upper

alphanum :: Parser Char
alphanum = letter <||> digit

word :: Parser String
word = many' letter

space :: Parser ()
space = do sat isSpace
           return ()

spaces :: Parser ()
spaces = do many' space
            return ()

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

comment :: Parser ()
comment = do string "--"
             many' (sat (\x -> x /= '\n'))
             return ()

junk :: Parser ()
junk = do many' (space <|> comment)
          return ()

token :: Parser a -> Parser a
token p = do { x <- p; junk; return x }

parse :: Parser a -> Parser a
parse p = do { junk; p }

symb :: String -> Parser String
symb cs = token (string cs)

ident :: Parser String
ident = do x  <- lower
           xs <- first (many' alphanum)
           return (x:xs)

many1 :: Parser a -> Parser [a]
many1 p = do x  <- p
             xs <- many' p
             return (x:xs)

nat :: Parser Int
nat = fmap read (many1 digit)

int :: Parser Int
int = do f <- op
         n <- nat
         return (f n)
      where op = do { char '-'; return negate } <|> return id

integer :: Parser Int
integer = token int

natural :: Parser Int
natural = token nat

identifier :: [String] -> Parser String
identifier ks = do x <- token ident
                   if not (elem x ks)
                   then return x
                   else mzero

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do x  <- p
                  xs <- many' (do { sep; p })
                  return (x:xs)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <||> return []

brackets :: Parser a -> Parser b -> Parser c -> Parser b
brackets open p close = do open
                           x <- p
                           close
                           return x

ints :: Parser [Int]
ints = brackets (char '[') (int `sepBy1` (char ',')) (char ']')

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
             where rest x = (do f <- op
                                y <- p
                                rest (f x y)) <||> return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
             where rest x = (do f <- op
                                y <- chainr1 p op
                                return (f x y)) <||> return x

addOp :: Num a => Parser (a -> a -> a)
addOp = (do { symb "+"; return (+) }) <||> 
        (do { symb "-"; return (-) })

mulOp :: Integral a => Parser (a -> a -> a)
mulOp = (do { symb "*"; return (*) }) <||>
        (do { symb "/"; return (div) }) 

expr :: Parser Int
expr = term `chainl1` addOp

term :: Parser Int
term = factor `chainl1` mulOp

factor :: Parser Int
factor = (token int) <||> (brackets (symb "(") expr (symb ")"))















