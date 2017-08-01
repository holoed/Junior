module Lib where

import Base
import Primitives
import Expr
import Data.Map hiding (null, map)
import Data.Char

predefEnv :: Env
predefEnv = fromList [
    ("==", Function (\(Value x) -> return $ Function (\(Value y) -> return $ Value (BoolVal (eq x y)) ))),
    ("/=", Function (\(Value x) -> return $ Function (\(Value y) -> return $ Value (BoolVal (not (eq x y))) ))),
    ("-", Function (\x -> return $ Function (\y -> return $ Value (sub (valueOf x) (valueOf y)) ))),
    ("+", Function (\x -> return $ Function (\y -> return $ Value (add (valueOf x) (valueOf y)) ))),
    ("*", Function (\x -> return $ Function (\y -> return $ Value (mul (valueOf x) (valueOf y)) ))),
    ("/", Function (\x -> return $ Function (\y -> return $ Value (divz (valueOf x) (valueOf y)) ))),
    (">", Function (\x -> return $ Function (\y -> return $ Value (gt (valueOf x) (valueOf y)) ))),
    ("||", Function (\(Value (BoolVal a)) -> return $ Function (\(Value (BoolVal b)) -> return $ Value (BoolVal (a || b)) ))),
    (":", Function (\x -> return $ Function (\(ListResult xs) -> return $ ListResult (x : xs)))),
    ("isEmpty", Function isEmpty),
    ("head", Function hd),
    ("tail", Function tl),
    ("empty", ListResult []),
    ("fst", Function (\(TupleResult xs) -> return $ head xs)),
    ("snd", Function (\(TupleResult xs) -> return $ head (tail xs))),
    ("neg", Function (\(Value (IntVal n)) -> return $ Value (IntVal (-n)))),
    ("mkTuple2", Function (\x -> return $ Function (\y -> return $ TupleResult [x, y]))),
    ("isLower", Function (\(Value (StringVal s)) -> return $ Value (BoolVal (isLower (head s))))),
    ("isUpper", Function (\(Value (StringVal s)) -> return $ Value (BoolVal (isUpper (head s))))),
    ("stringToCharList", Function (\(Value (StringVal s)) -> return $ ListResult (fmap (\x -> Value $ StringVal [x]) s))),
    ("charListToString", Function (\(ListResult xs) -> return $ Value (StringVal (concatMap (\(Value (StringVal x)) -> x) xs)))),
    ("stringToInt", Function (\(Value (StringVal s)) -> return $ Value (IntVal (read s)))),
    ("size", Function (\(Value (IntVal n)) -> return $ Value (IntVal (10 ^ length (show n))))),
    ("notElem", Function (\x -> return $ Function (\(ListResult xs) -> return $ Value (BoolVal (notElem x xs))))),
    ("evar", Function (\(Value (StringVal s)) -> return $ ExpResult (EVar s))),
    ("elit", Function (\(Value x) -> return $ ExpResult (ELit x))),
    ("elet", Function (\(Value (StringVal s)) -> return $ Function (\(ExpResult e1) -> return $ Function (\(ExpResult e2) -> return $ ExpResult (ELet s e1 e2))))),
    ("eapp", Function (\(ExpResult e1) -> return $ Function (\(ExpResult e2) -> return $ ExpResult (EApp e1 e2)))),
    ("elam", Function (\(Value (StringVal s)) -> return $ Function (\(ExpResult e) -> return $ ExpResult (ELam s e)))),
    ("eIfThenElse", Function (\(ExpResult p) -> return $ Function (\(ExpResult e1) -> return $ Function (\(ExpResult e2) -> return $ ExpResult (EIfThenElse p e1 e2))))),
    ("isvar", Function (\(ExpResult e) -> return $ Value (BoolVal (isVar e)))),
    ("islit", Function (\(ExpResult e) -> return $ Value (BoolVal (isLit e)))),
    ("islet", Function (\(ExpResult e) -> return $ Value (BoolVal (isLet e)))),
    ("isapp", Function (\(ExpResult e) -> return $ Value (BoolVal (isApp e)))),
    ("islam", Function (\(ExpResult e) -> return $ Value (BoolVal (isLam e)))),
    ("isifThenElse", Function (\(ExpResult e) -> return $ Value (BoolVal (isIfThenElse e)))),
    ("matchExp", Function (\(ExpResult e) -> return $
                 Function (\(Function litF) -> return $
                 Function (\(Function varF) -> return $
                 Function (\(Function lamF) -> return $
                 Function (\(Function appF) -> return $
                 Function (\(Function letF) -> return $
                 Function (\(Function ifThenElseF) ->
                  case e of
                    ELit n -> litF (Value n)
                    EVar s -> varF (Value (StringVal s))
                    ELam s e1 -> do (Function g) <- lamF (Value (StringVal s))
                                    g (ExpResult e1)
                    EApp e1 e2 -> do (Function g) <- appF (ExpResult e1)
                                     g (ExpResult e2)
                    ELet s e1 e2 -> do (Function g) <- letF (Value (StringVal s))
                                       (Function h) <- g (ExpResult e1)
                                       h (ExpResult e2)
                    EIfThenElse p e1 e2 -> do (Function g) <- ifThenElseF (ExpResult p)
                                              (Function h) <- g (ExpResult e1)
                                              h (ExpResult e2)
                    ))))))))

  ]

eq (IntVal x) (IntVal y) = x == y
eq (StringVal x) (StringVal y) = x == y

isEmpty :: Result -> EnvReader Result
isEmpty (ListResult xs) = return $ Value (BoolVal (null xs))
isEmpty (Value (StringVal xs)) = return $ Value (BoolVal (null xs))

hd :: Result -> EnvReader Result
hd (ListResult xs) = return $ head xs
hd (Value (StringVal xs)) = return $ Value $ StringVal [head xs]

tl :: Result -> EnvReader Result
tl (ListResult xs) = return $ ListResult (tail xs)
tl (Value (StringVal xs)) = return $ Value $ StringVal (tail xs)

add :: Prim -> Prim -> Prim
add (IntVal x) (IntVal y) = IntVal (x + y)
add (IntVal x) (FloatVal y) = FloatVal (fromIntegral x + y)

sub :: Prim -> Prim -> Prim
sub (IntVal x) (IntVal y) = IntVal (x - y)
sub (IntVal x) (FloatVal y) = FloatVal (fromIntegral x - y)

mul :: Prim -> Prim -> Prim
mul (IntVal x) (IntVal y) = IntVal (x * y)
mul (IntVal x) (FloatVal y) = FloatVal (fromIntegral x * y)

divz :: Prim -> Prim -> Prim
divz (IntVal x) (IntVal y) = FloatVal (fromIntegral x / fromIntegral y)

gt :: Prim -> Prim -> Prim
gt (IntVal x) (IntVal y) = BoolVal (x > y)
