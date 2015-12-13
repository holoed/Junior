module Compiler.TypeInference.Environments where

import Compiler.TypeInference.TypeTree
import Data.Set as Data (Set, singleton, union, empty, difference)
import Data.Map as Data (Map, elems)

data TyScheme = TyScheme (Type, Data.Set String)

data Env = Env (Map String TyScheme)

-- Calculate the list of type variables occurring in a type without repeats.
getTVarsOfType :: Type -> Set String
getTVarsOfType (TyVar n) = singleton n
getTVarsOfType (TyLam t1 t2) = getTVarsOfType t1 `union` getTVarsOfType t2
getTVarsOfType (TyCon (_, args)) = foldl (\ acc t -> acc `union` getTVarsOfType t) empty args

getTVarsOfScheme :: TyScheme -> Set String
getTVarsOfScheme (TyScheme (t, tvars)) = getTVarsOfType t `difference` tvars

getTVarsOfEnv :: Env -> Set String
getTVarsOfEnv (Env e) = foldl (\ acc s -> acc `union` getTVarsOfScheme s) empty (elems e)
