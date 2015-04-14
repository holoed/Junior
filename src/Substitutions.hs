module Substitutions where

import Data.Map (Map, insert)
import TypeTree
import qualified Data.Map as Data.Map (lookup)
import Utils

data Subst = Subst (Map String Type) 

extend :: String -> Type -> Subst -> Subst
extend v t (Subst s) = Subst(insert v t s)

lookup :: String -> Subst -> Type
lookup v (Subst s) = case (Data.Map.lookup v s) of
                       Just x -> x
                       Nothing -> TyVar v

subs :: Type -> Subst -> Type
subs t@(TyVar n) s = let t' = Substitutions.lookup n s in
                     if (t == t') then t'
                     else subs t' s      
subs (TyLam a r) s = TyLam (subs a s) (subs r s)
subs (TyCon (name, tyArgs)) s = TyCon (name, tyArgs |> map (\x -> subs x s))      