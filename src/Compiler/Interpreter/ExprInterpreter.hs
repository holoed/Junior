module Compiler.Interpreter.ExprInterpreter where

import Compiler.Ast
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Map
import Data.Maybe

executionEnvironment:: Map String Result
executionEnvironment = fromList[
  ("+", Function(\(Const (Int x), _) -> Function(\(Const (Int y), _) -> Const(Int (x + y))))),
  ("*", Function(\(Const (Int x), _) -> Function(\(Const (Int y), _) -> Const(Int (x * y))))),
  ("-", Function(\(Const (Int x), _) -> Function(\(Const (Int y), _) -> Const(Int (x - y))))),
  ("==", Function(\(Const (Int x), _) -> Function(\(Const (Int y), _) -> Const(Bool (x == y)))))
  ]

type Interpreter a = StateT (Map String Result) Identity a

data Result = Const Lit
            | Function ((Result, Map String Result) -> Result)

instance Show Result where
  show (Const x) = show x
  show _ = fail "Cannot show function"

instance Eq Result where
  (==) (Const x) (Const y) = x == y
  (==) _ _ = undefined

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
evalExpr (Lam s e) = return (Function (\(x, env) -> evalState (evalExpr e) (insert s x env)))
evalExpr (App e1 e2) = do (Function f) <- evalExpr e1
                          x <- evalExpr e2
                          env <- get
                          return (f (x, env))
evalExpr (Var n) = do env <- get
                      let v = Data.Map.lookup n env
                      maybe (error ("Cannot find " ++ show n ++ " in environment")) return v
evalExpr (Let bs e) = do _ <- evalDecls (fmap (uncurry DeclValue) bs)
                         evalExpr e
evalExpr (IfThenElse e1 e2 e3) = do (Const (Bool b)) <- evalExpr e1
                                    if b then evalExpr e2
                                         else evalExpr e3
