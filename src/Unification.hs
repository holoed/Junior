module Unification where

import TypeTree
import Substitutions
import Environments
import Data.Set (member)
import Utils

-- Calculate the most general unifier of two types.
mgu :: Type -> Type -> Subst -> Subst
mgu a b s = case (subs a s, subs b s) of
               (TyVar ta, TyVar tb) | ta == tb -> s
               (TyVar ta, _) | (not (member ta (getTVarsOfType b))) -> extend ta b s
               (_, TyVar _) -> mgu b a s
               (TyLam a1 b1,TyLam a2 b2) -> mgu a1 a2 (mgu b1 b2 s)
               (TyCon (name1, args1), TyCon(name2, args2)) | name1 == name2 -> 
                        (s, args1, args2) |||> fold2 (\ subst t1 t2 -> mgu t1 t2 subst)
               (x, y) -> error ("Unification error of " ++ (show x) ++ " and " ++ (show y))