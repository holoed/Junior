module Compiler.Utils where

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

(||>) :: (a, b) -> (a -> b -> c) -> c
(||>) (x,y) f = f x y

(|||>) :: (a, b, c) -> (a -> b -> c -> d) -> d
(|||>) (x,y,z) f = f x y z

fold2 :: (a -> b -> c -> a) -> a -> [b] -> [c] -> a
fold2 _ acc [] [] = acc
fold2 f acc (x:xs) (y:ys) = fold2 f (f acc x y) xs ys
fold2 _ _ (_:_) [] = error "Sequences not of same length"
fold2 _ _  [] (_:_) = error "Sequences not of same length"

foldM2 :: (Monad m) => (a -> b -> c -> m a) -> a -> [b] -> [c] -> m a
foldM2 _ acc [] [] =  return acc
foldM2 f acc (x:xs) (y:ys) = f acc x y >>= \faxy -> foldM2 f faxy xs ys
foldM2 _ _ (_:_) [] = error "Sequences not of same length"
foldM2 _ _  [] (_:_) = error "Sequences not of same length"
