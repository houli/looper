module Program
  ( Program(..)
  , Run
  , run
  ) where

import           Control.Monad (when)
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Map as Map
import           System.IO (hFlush, stdout)
import           Text.Read hiding (get)

import           Eval

data Program = Assign String Expr
             | If Expr Program Program
             | While Expr Program
             | Print Expr
             | Program :> Program -- Sequencing
             | Try Program Program
             deriving (Eq, Read, Show)

data Command = Inspect
             | Step
             | Where
             deriving (Eq, Read, Show)

type Run a = StateT Env (ExceptT String IO) a

runRun :: Run a -> IO (Either String (a, Env))
runRun p = runExceptT (runStateT p Map.empty)

run :: Program -> IO ()
run program = do result <- runRun $ exec program
                 case result of
                   Left exn -> print ("Uncaught exception: " ++ exn)
                   Right env -> print env

prompt :: Run String
prompt = liftIO $ putStr "looper> " *> hFlush stdout *> getLine

readCommand :: Program -> Run ()
readCommand p = do
  input <- prompt
  case readMaybe input of
    Nothing -> do
      liftIO $ putStrLn ("Invalid command: " ++ input)
      readCommand p
    Just command -> runCommand p command

runCommand :: Program -> Command -> Run ()
runCommand p Inspect = do
  st <- get
  liftIO $ print st
  readCommand p
runCommand p Step = exec p
runCommand p Where = execStepped p

execStepped :: Program -> Run ()
execStepped p = do
  liftIO $ print p
  readCommand p

set :: (Name, Val) -> Run ()
set (s, i) = state (\table -> ((), Map.insert s i table))

exec :: Program -> Run ()
exec (Assign s v) = do st <- get
                       Right val <- pure $ runEval st (eval v)
                       set (s, val)
exec (If cond s0 s1) = do st <- get
                          Right (B val) <- pure $ runEval st (eval cond)
                          if val then execStepped s0 else execStepped s1
exec (While cond s) = do st <- get
                         Right (B val) <- pure $ runEval st (eval cond)
                         when val $ execStepped s *> execStepped (While cond s)
exec (Print e) = do st <- get
                    Right val <- pure $ runEval st (eval e)
                    liftIO $ print val
                    pure ()
exec (s0 :> s1) = execStepped s0 >> execStepped s1
exec (Try s0 s1) = catchError (execStepped s0) (\e -> execStepped s1)
