module Interpreter
  ( Run
  , run
  ) where

import           Control.Monad (when)
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map as Map
import           System.IO (hFlush, stdout)

import           Command
import           Eval
import           Program

data ProgramState = ProgramState
  { env :: Env
  , previous :: [Program]
  } deriving Show

type Run a = StateT ProgramState (ExceptT String IO) a

runRun :: Run a -> IO (Either String ProgramState)
runRun p = runExceptT (execStateT p $ ProgramState Map.empty [])

-- Remove the "fromList" at the start
printEnvironment :: Env -> IO ()
printEnvironment = putStrLn . drop 9 . show

run :: Program -> IO ()
run program = do
  result <- runRun $ exec program
  case result of
    Left exn -> print ("Uncaught exception: " ++ exn)
    Right state -> printEnvironment $ env state

readCommand :: Program -> Run ()
readCommand p = do
  input <- prompt
  case parseCommand input of
    Nothing -> do
      liftIO $ putStrLn ("Invalid command: " ++ input)
      readCommand p
    Just command -> runCommand command p
  where prompt = liftIO $ putStr "looper> " *> hFlush stdout *> getLine

runCommand :: Command -> Program -> Run ()
runCommand Inspect = runInspect
runCommand (InspectVariable name) = runInspectVariable name
runCommand Step = exec
runCommand Back = execPrevious
runCommand Location = execStepped
runCommand Debug = \p -> do
  previous <- gets previous
  liftIO $ print previous
  readCommand p

runInspect :: Program -> Run ()
runInspect p = do
  env <- gets env
  liftIO $ printEnvironment env
  readCommand p

runInspectVariable :: String -> Program -> Run ()
runInspectVariable name p = do
  env <- gets env
  case Map.lookup name env of
    Nothing -> liftIO $ putStrLn ("Unknown variable " ++ name)
    Just val -> liftIO $ print val
  readCommand p

execPrevious :: Program -> Run ()
execPrevious p = do
  pState <- get
  case previous pState of
    [] -> liftIO (putStrLn "Can't go further back") *> execStepped p
    (x:xs) -> do
      put pState { previous = xs }
      execStepped x

execStepped :: Program -> Run ()
execStepped p@(Assign _ _) = do
  liftIO $ print p
  readCommand p
execStepped p@(Print _) = do
  liftIO $ print p
  readCommand p
execStepped p = exec p

set :: (Name, Val) -> Run ()
set (s, i) = do
  pState <- get
  put pState { env = Map.insertWith (++) s [i] (env pState) }

addProgram :: Program -> Run ()
addProgram p = do
  pState <- get
  put pState { previous = p : previous pState }

exec :: Program -> Run ()
exec p@(Assign s e) = do env <- gets env
                         Right val <- pure $ runEval env (eval e)
                         set (s, val)
                         addProgram p
exec p@(If cond s0 s1) = do env <- gets env
                            Right (B val) <- pure $ runEval env (eval cond)
                            if val then execStepped s0 else execStepped s1
                            addProgram p
exec p@(While cond s) = do env <- gets env
                           Right (B val) <- pure $ runEval env (eval cond)
                           when val $ execStepped s *> execStepped p
                           addProgram p
exec p@(Print e) = do env <- gets env
                      Right val <- pure $ runEval env (eval e)
                      liftIO $ print val
                      addProgram p
exec p@(s0 :> s1) = execStepped s0 *> execStepped s1 *> addProgram p
