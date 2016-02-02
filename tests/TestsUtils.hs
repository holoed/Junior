module TestsUtils where

import Data.Maybe (maybeToList)
import Compiler.Ast
import Compiler.TypeInference.TypeTree
import Compiler.TypeInference.TypeChecker
import Compiler.Parser.BaseParsers
import Compiler.Parser.JuniorParser
import Compiler.Interpreter.JuniorInterpreter
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Identity

runParser :: Parser a -> PString -> [(a, PString)]
runParser m (p, s) = maybeToList $ runIdentity (runMaybeT (runStateT (runReaderT m p) (p,s)))

typeCheck :: String -> [(String, Type)]
typeCheck s = do (ds, _) <- runParser globalDef ((0, 0), s)
                 typeOfProg (Prog ds)

runProg :: String -> ([(String, Type)], [Result])
runProg s = (ty, evalState (evalProg (Prog ds)) executionEnvironment)
  where [(ds, _)] = runParser globalDef ((0, 0), s)
        prog = Prog ds
        ty = typeOfProg prog
