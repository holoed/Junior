module Compiler.TypeInference.Substitutions where

import Data.Maybe (fromMaybe)
import Data.Map (Map, insert, lookup)
import Compiler.TypeInference.TypeTree
import Compiler.Utils

data Subst = Subst (Map String Type)

extend :: String -> Type -> Subst -> Subst
extend v t (Subst s) = Subst(insert v t s)

lookup :: String -> Subst -> Type
lookup v (Subst s) = fromMaybe (TyVar v) (Data.Map.lookup v s)

subs :: Type -> Subst -> Type
subs t@(TyVar n) s = let t' = Compiler.TypeInference.Substitutions.lookup n s in
                     if t == t' then t'
                     else subs t' s
subs (TyLam a r) s = TyLam (subs a s) (subs r s)
subs (TyCon (name, tyArgs)) s = TyCon (name, tyArgs |> map (`subs` s))
subs (TyPoly t) s = subs t s
