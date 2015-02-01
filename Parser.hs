{-# LANGUAGE InstanceSigs #-}

module Parser where

import Control.Applicative
import Control.Monad

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
	m >>= f = bind m f

instance MonadPlus Parser where
	mzero :: Parser a
	mzero = zero
	mplus :: Parser a -> Parser a -> Parser a
	mplus = plus 

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
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

many1 :: Parser a -> Parser [a]
many1 p = do x  <- p
             xs <- many p
             return (x:xs)

nat :: Parser Int
nat = fmap read (many1 digit)

int :: Parser Int
int = do f <- op
         n <- nat
         return (f n)
      where op = do { char '-'; return negate } <|> return id

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do x  <- p
                  xs <- many (do { sep; p })
                  return (x:xs)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

brackets :: Parser a -> Parser b -> Parser c -> Parser b
brackets open p close = do open
                           x <- p
                           close
                           return x

ints :: Parser [Int]
ints = brackets (char '[') (int `sepBy1` (char ',')) (char ']')

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = bind p rest
             where rest x = (do f <- op
                                y <- p
                                rest (f x y)) <|> return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = bind p rest
             where rest x = (do f <- op
                                y <- chainr1 p op
                                return (f x y)) <|> return x

addOp :: Num a => Parser (a -> a -> a)
addOp = (do { char '+'; return (+) }) <|> 
        (do { char '-'; return (-) })

mulOp :: Integral a => Parser (a -> a -> a)
mulOp = (do { char '*'; return (*) }) <|>
        (do { char '/'; return (div) }) 

expr :: Parser Int
expr = term `chainl1` addOp

term :: Parser Int
term = factor `chainl1` mulOp

factor :: Parser Int
factor = int <|> (brackets (char '(') expr (char ')'))

force :: Parser a -> Parser a
force p = P (\inp -> let x = apply p inp in (fst (head x), snd (head x)) : tail x)

first :: Parser a -> Parser a
first p = P (\inp -> case apply p inp of
	                   [] -> []
	                   (x:xs) -> [x])





