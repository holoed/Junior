module TypeTree where

import Text.Printf

data Type = TyLam Type Type
          | TyVar String
          | TyCon (String, [Type])
          deriving (Show, Eq)

typeToString :: Type -> String
typeToString (TyLam t1 t2) =  printf "(%s -> %s)" (typeToString t1) (typeToString t2)
typeToString (TyVar a) = a
typeToString (TyCon (s, _)) = s