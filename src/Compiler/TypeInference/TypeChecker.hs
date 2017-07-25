module Compiler.TypeInference.TypeChecker where

import Control.Monad.State
import Compiler.TypeInference.TypeTree
import Compiler.Ast
import Compiler.TypeInference.Environments
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Compiler.Utils
import Compiler.TypeInference.Unification
import Compiler.TypeInference.Substitutions
import Data.Set as Set (empty, difference)
import Compiler.TypeInference.AlphaConverter
import GHC.List as List (foldl)

newTyVar :: State Int Type
newTyVar = do x <- get
              put (x + 1)
              return (TyVar ("T" ++ show x))

integerCon :: Type
integerCon = TyCon("int", [])

floatCon :: Type
floatCon = TyCon("float", [])

charCon :: Type
charCon = TyCon("char", [])

stringCon :: Type
stringCon = TyCon("string", [])

boolCon :: Type
boolCon = TyCon("bool", [])

litToTy :: Lit -> Type
litToTy (Int _) = integerCon
litToTy (Float _) = floatCon
litToTy (Char _) = charCon
litToTy (String  _) = stringCon
litToTy (Bool _) = boolCon

findSc :: String -> Env -> TyScheme
findSc n (Env e) = e |> Map.lookup n |> fromMaybe (error ("Cannot find type scheme for " ++ show n) :: TyScheme)

containsSc :: String -> Env -> Bool
containsSc n (Env e) = Map.member n e

addSc :: String -> TyScheme -> Env -> Env
addSc n sc (Env e) = Env (Map.insert n sc e)

-- Calculate the principal type scheme for an expression in a given
-- typing environment
tp :: Env -> Expr -> Type -> Subst -> State Int Subst
tp env e bt s =
        case e of
        (Literal v) -> return (mgu (litToTy v) bt s)

        (Var n) -> do unless (containsSc n env) $ error ("Name " ++ n ++ " not found")
                      let (TyScheme (t, _)) = findSc n env
                      return (mgu (subs t s) bt s)

        (Lam x e') -> do a <- newTyVar
                         b <- newTyVar
                         let s1 = mgu bt (TyLam a b) s
                         let newEnv = addSc x (TyScheme (a, Set.empty)) env
                         tp newEnv e' b s1

        (App e1 e2) -> do a <- newTyVar
                          s1 <- tp env e1 (TyLam a bt) s
                          tp env e2 a s1

        (Let [] body) -> tp env body bt s

        (Let ((name, inV):xs) body) -> do a <- fmap TyPoly newTyVar
                                          s1 <- tp env inV a s
                                          let t = subs a s1
                                          let newScheme = TyScheme (t, getTVarsOfType t `Set.difference` getTVarsOfEnv env)
                                          let newEnv = addSc name newScheme env
                                          tp  newEnv (Let xs body) bt s1

        (IfThenElse e1 e2 e3) -> do s1 <- tp env e1 boolCon s
                                    s2 <- tp env e2 bt s1
                                    let t = subs bt s2
                                    tp env e3 t s2


predefinedEnv :: Env
predefinedEnv =  Env([("+", TyScheme (TyLam integerCon (TyLam integerCon integerCon), Set.empty)),
                      ("*", TyScheme (TyLam integerCon (TyLam integerCon integerCon), Set.empty)),
                      ("-", TyScheme (TyLam integerCon (TyLam integerCon integerCon), Set.empty)),
                      ("==", TyScheme (TyLam integerCon (TyLam integerCon boolCon), Set.empty)),
                      (">", TyScheme (TyLam integerCon (TyLam integerCon boolCon), Set.empty)),
                      ("<", TyScheme (TyLam integerCon (TyLam integerCon boolCon), Set.empty))] |> Map.fromList)

typeOf :: Expr -> Type
typeOf = typeOfInEnv predefinedEnv

typeOfInEnv :: Env -> Expr -> Type
typeOfInEnv env e = evalState typeOf' 1 |> renameTVarsToLetters
     where typeOf' = do a <- newTyVar
                        let emptySubst = Subst Map.empty
                        s1 <- tp env e a emptySubst
                        return (subs a s1)

typeOfProg :: Prog -> [(String, Type)]
typeOfProg (Prog decls) = snd $ List.foldl typeCheckDecl (predefinedEnv, []) decls
   where
         typeCheckDecl :: (Env, [(String, Type)]) -> Decl -> (Env, [(String, Type)])
         typeCheckDecl (env, ts) (DeclValue name e) = let newEnv = addSc name (TyScheme (TyVar "T0", Set.empty)) env
                                                          t = typeOfInEnv newEnv e
                                                          newEnv' = addSc name (TyScheme (t, Set.empty)) env
                                                          in (newEnv', (name, t) : ts)
