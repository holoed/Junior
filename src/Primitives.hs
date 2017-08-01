module Primitives where

data Prim = IntVal Int
          | FloatVal Float
          | StringVal String
          | BoolVal Bool
          | UnitVal deriving (Show, Eq)
