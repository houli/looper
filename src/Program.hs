module Program
  ( Program(..)
  , Run
  , run
  ) where

import           Control.Monad          (when)
import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map as Map

import Eval

data Program = Assign String Expr
             | If Expr Program Program
             | While Expr Program
             | Print Expr
             | Program :> Program -- Sequencing
             | Try Program Program
             deriving (Eq, Read, Show)

type Run a = StateT Env (ExceptT String IO) a

runRun :: Run a -> IO (Either String (a, Env))
runRun p = runExceptT (runStateT p Map.empty)

set :: (Name, Val) -> Run ()
set (s, i) = state (\table -> ((), Map.insert s i table))

exec :: Program -> Run ()
exec (Assign s v) = do st <- get
                       Right val <- pure $ runEval st (eval v)
                       set (s, val)
exec (If cond s0 s1) = do st <- get
                          Right (B val) <- pure $ runEval st (eval cond)
                          if val then exec s0 else exec s1
exec (While cond s) = do st <- get
                         Right (B val) <- pure $ runEval st (eval cond)
                         when val $ exec s *> exec (While cond s)
exec (Print e) = do st <- get
                    Right val <- pure $ runEval st (eval e)
                    liftIO $ print val
                    pure ()
exec (s0 :> s1) = exec s0 >> exec s1
exec (Try s0 s1) = catchError (exec s0) (\e -> exec s1)

run :: Program -> IO ()
run program = do result <- runRun $ exec program
                 case result of
                   Left exn -> print ("Uncaught exception: " ++ exn)
                   Right env -> print env
