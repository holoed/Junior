{-# LANGUAGE InstanceSigs #-}

module Parser where

import Control.Applicative

data Parser a = P (String -> [(a, String)])

result :: a -> Parser a 
result v = P(\inp -> [(v, inp)])

zero :: Parser a
zero = P(\inp -> [])

item :: Parser Char
item = P(\inp -> case inp of
	              [] -> []
	              (x:xs) -> [(x, xs)])

apply :: Parser a -> String -> [(a, String)]
apply (P p) s = p s

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = P (\inp -> concat [apply (f v) inp' | (v, inp') <- apply p inp])

plus :: Parser a -> Parser a -> Parser a
plus p q = P (\inp -> (apply p inp ++ apply q inp))

instance Functor Parser where
	fmap :: (a -> b) -> Parser a -> Parser b
	fmap f m = bind m (\x -> result (f x))

instance Applicative Parser where
	pure :: a -> Parser a
	pure = return

	(<*>) :: Parser (a -> b) -> Parser a -> Parser b
	(<*>) mf mx = bind mf (\f -> bind mx (\x -> result (f x)))

instance Alternative Parser where
	empty :: Parser a
	empty = zero

	(<|>) :: Parser a -> Parser a -> Parser a
	(<|>) = plus

instance Monad Parser where
	return :: a -> Parser a
	return = result

	(>>=) :: Parser a -> (a -> Parser b) -> Parser b
	(>>=) = bind

seq :: Parser a -> Parser b -> Parser (a, b)
seq p q = do  x <- p
              y <- q
              return (x, y)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if (p x) then
           	   return x
           else
           	   zero

char :: Char -> Parser Char
char ch = sat (==ch)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')


letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit

word :: Parser String
word = many letter

string :: String -> Parser String
string [] = return []
string (x:xs) = do _ <- char x
                   _ <- string xs
                   return (x:xs)


