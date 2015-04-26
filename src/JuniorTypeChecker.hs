module JuniorTypeChecker where

import Control.Monad.State 
import TypeTree
import Ast
import Environments
import Data.Map as Map
import Data.Maybe (fromJust)
import Utils
import Unification
import Substitutions
import Data.Set as Set (empty, difference)
import AlphaConverter

newTyVar :: State Int Type
newTyVar = do x <- get
              put (x + 1)
              return (TyVar ("T" ++ (show x)))

integerCon :: Type
integerCon = TyCon("int", [])

floatCon :: Type
floatCon = TyCon("float", [])

charCon :: Type
charCon = TyCon("char", [])

stringCon :: Type
stringCon = TyCon("string", [])        

litToTy :: Lit -> Type
litToTy (Int _) = integerCon
litToTy (Float _) = floatCon
litToTy (Char _) = charCon
litToTy (String  _) = stringCon      

findSc :: String -> Env -> TyScheme
findSc n (Env e) = e |> Map.lookup n |> fromJust

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
        (Var n) -> do if (not (containsSc n env)) then error ("Name " ++ n ++ " not found") else return ()
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

        (Let [(name, inV)] body) -> do a <- newTyVar
                                       s1 <- tp env inV a s
                                       let t = subs a s1
                                       let newScheme = TyScheme (t, ((getTVarsOfType t) `Set.difference` (getTVarsOfEnv env)))
                                       tp (addSc name newScheme env) body bt s1

        (Decl [(name, body)]) -> do a <- newTyVar
                                    s1 <- tp env body bt s
                                    let t = subs a s1
                                    return (extend name t s1)

        _ -> fail "Not currently supported"

predefinedEnv :: Env
predefinedEnv =  Env([("+", TyScheme ((TyLam integerCon (TyLam integerCon integerCon)), Set.empty)),
                      ("*", TyScheme ((TyLam integerCon (TyLam integerCon integerCon)), Set.empty)),
                      ("-", TyScheme ((TyLam integerCon (TyLam integerCon integerCon)), Set.empty))] |> Map.fromList)

typeOf :: Expr -> Type
typeOf e = evalState (typeOf') 0 |> renameTVarsToLetters
     where typeOf' = do a <- newTyVar
                        let emptySubst = Subst (Map.empty)
                        s1 <- tp predefinedEnv e a emptySubst
                        return (subs a s1)