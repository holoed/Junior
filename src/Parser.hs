{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}

module Parser where

import Prelude hiding (seq)
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader

type Pos = (Int, Int)

type PString = (Pos, String)

type Parser a = ReaderT Pos (StateT PString []) a

item :: Parser Char
item = do s <- (lift get)
          dpos <- ask
          case s of 
          	(_, []) -> mzero
          	(pos, x:xs) ->  if (onside pos dpos)
                                then do lift (put (newState (pos, x:xs)))
          	                        return x 
          	                else mzero

onside :: Pos -> Pos -> Bool
onside (l, c) (dl, dc) = (c > dc) || (l == dl)


newState :: PString -> PString
newState (pos, []) = (pos, [])
newState ((l, c), x:xs) = (newpos, xs)
	where newpos = case x of 
		            '\n' -> (l + 1, 0)
		            '\t' -> (l, ((c `div` 8) + 1) * 8)
		            _ -> (l, c + 1)

many1_offside :: Parser a -> Parser [a]
many1_offside p = do (pos, _) <- (lift get)
                     vs <- local (\_ -> pos) (many1 (off p))
                     return vs

off :: Parser a -> Parser a
off p = do (_, dc) <- ask
           ((l, c), _) <- (lift get)
           guard (c == dc)
           v <- local (\_ -> (l, dc)) p
           return v

first :: Parser a -> Parser a
first p = mapReaderT (\m -> mapStateT (\xs -> take 1 xs) m) p

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
junk = do local (\_ -> (0, -1)) (many' (space <|> comment))
          return ()

token :: Parser a -> Parser a
token p = do { x <- p; junk; return x }

parse :: Parser a -> Parser a
parse p = do { junk; p }

symbol :: String -> Parser String
symbol cs = token (string cs)

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

float :: Parser Float
float = do m <- int
           symbol "."
           n <- nat
           return (read ((show m) ++ "." ++ (show n))) -- TODO: Refactor to more efficient way.

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

















