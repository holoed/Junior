{-# LANGUAGE UndecidableInstances #-}

module Experiments.RecursionSchemes where

fix :: ((a -> b) -> a -> b) -> a -> b
fix f = f (fix f)

data Fix f = In { out :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
  show (In f) = "(" ++ show f ++ ")"

cata :: Functor f => (f a -> a) -> (Fix f -> a) -> Fix f -> a
cata psi f = psi . fmap f . out

cataRec :: Functor f => (f a -> a) -> Fix f -> a
cataRec psi = fix (cata psi)
