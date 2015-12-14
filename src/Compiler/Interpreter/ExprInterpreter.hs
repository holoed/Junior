module Compiler.Interpreter.ExprInterpreter where

import Compiler.Ast
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Map
import Data.Maybe

makeFunction :: (Int -> Int -> a) -> (a -> Lit) -> Result
makeFunction op mkLit = Function(\(Const (Int x)) ->
                        return (Function(\(Const (Int y)) ->
                        return (Const(mkLit (x `op` y))))))

executionEnvironment:: Map String Result
executionEnvironment = fromList[
  ("+", makeFunction (+) Int),
  ("*", makeFunction (*) Int),
  ("-", makeFunction (-) Int),
  ("==", makeFunction (==) Bool)
  ]

type Interpreter a = StateT (Map String Result) Identity a

data Result = Const Lit
            | Function (Result -> Interpreter Result)

instance Show Result where
  show (Const x) = show x
  show _ = error "Cannot show a result function"

instance Eq Result where
  (==) (Const x) (Const y) = x == y
  (==) x y = error ("failed to compare " ++ show x ++ " and" ++ show y)

evalProg :: Prog -> Interpreter [Result]
evalProg (Prog decls) = do _ <- evalDecls decls
                           env <- get
                           return (maybeToList (Data.Map.lookup "main" env))

evalDecls :: [Decl] -> Interpreter [Result]
evalDecls = mapM evalDecl

evalDecl :: Decl -> Interpreter Result
evalDecl (DeclValue s e) = do
                           env <- get
                           r <- evalExpr e
                           put (insert s r env)
                           return r

evalExpr :: Expr -> Interpreter Result
evalExpr (Literal lit) = return (Const lit)
evalExpr (Lam s e) = return (Function (\x -> do env <- get
                                                put (insert s x env)
                                                evalExpr e))
evalExpr (App e1 e2) = do (Function f) <- evalExpr e1
                          x <- evalExpr e2
                          f x
evalExpr (Var n) = do env <- get
                      let v = Data.Map.lookup n env
                      maybe (error ("Cannot find " ++ show n ++ " in environment")) return v
evalExpr (Let bs e) = do _ <- evalDecls (fmap (uncurry DeclValue) bs)
                         evalExpr e
evalExpr (IfThenElse e1 e2 e3) = do (Const (Bool b)) <- evalExpr e1
                                    if b then evalExpr e2
                                         else evalExpr e3
