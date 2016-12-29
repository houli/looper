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
import           Instruction
import           Program (Program)

data ProgramState = ProgramState
  { env :: Env
  , previous :: [Instruction]
  , index :: Int
  , enterCommand :: Command
  } deriving Show

type Run a = StateT ProgramState (ExceptT String IO) a

runRun :: Run a -> IO (Either String ProgramState)
runRun p = runExceptT (execStateT p $ ProgramState Map.empty [] 0 Step)

-- Remove the "fromList" at the start
printEnvironment :: MonadIO m => Env -> m ()
printEnvironment = liftIO . putStrLn . drop 9 . show

run :: Program -> IO ()
run program = do
  result <- runRun $ runInstructions $ fromProgram program
  case result of
    Left exn -> print ("Uncaught exception: " ++ exn)
    Right state -> printEnvironment $ env state

runInstructions :: [Instruction] -> Run ()
runInstructions ins = do
  index <- gets index
  when (index < length ins) $ do
    let i = ins !! index
    execStepped i
    runInstructions ins

execStepped :: Instruction -> Run ()
execStepped i = if isJump i then runStep i -- Skip command prompt for control flow
                else do
                  liftIO $ print i
                  readCommand i

exec :: Instruction -> Run Int
exec (Assign s e) = do
  val <- getValue e
  set (s, val)
  pure 1
exec (JMP offset) = pure offset
exec (JMPF cond offset) = do
  B cond <- getValue cond
  if cond then pure 1 else pure offset
exec (Print e) = do
  val <- getValue e
  liftIO $ print val
  pure 1

back :: Instruction -> Run Int
back (Assign s _) = do
  env <- gets env
  let Just (val:vals) = Map.lookup s env
  let newEnv = if null vals then Map.delete s env
               else Map.insert s vals env
  modify $ \pState -> pState { env = newEnv }
  pure 1
back i@JMP{} = exec i
back i@JMPF{} = exec i
back Print{} = pure 1

getValue :: Expr -> Run Val
getValue e = do
  env <- gets env
  Right val <- pure $ runEval env (eval e)
  pure val

set :: (Name, Val) -> Run ()
set (s, val) = modify $ \pState -> pState { env = Map.insertWith (++) s [val] (env pState) }

readCommand :: Instruction -> Run ()
readCommand i = do
  input <- prompt
  if null input then do
    lastCommand <- gets enterCommand
    runCommand lastCommand i -- Repeat step/back command if user presses enter
  else
    case parseCommand input of
      Nothing -> do
        liftIO $ putStrLn ("Invalid command: " ++ input)
        readCommand i
      Just command -> runCommand command i
  where prompt = liftIO $ putStr "looper> " *> hFlush stdout *> getLine

runCommand :: Command -> Instruction -> Run ()
runCommand Inspect = runInspect
runCommand (InspectVariable name) = runInspectVariable name
runCommand Step = runStep
runCommand Back = runBack
runCommand Location = execStepped

runInspect :: Instruction -> Run ()
runInspect i = do
  env <- gets env
  printEnvironment env
  readCommand i

runInspectVariable :: String -> Instruction -> Run ()
runInspectVariable name i = do
  env <- gets env
  liftIO $ case Map.lookup name env of
    Nothing -> putStrLn ("Unknown variable " ++ name)
    Just val -> print val
  readCommand i

runStep :: Instruction -> Run ()
runStep i = do
  offset <- exec i
  modify $ \pState -> pState { index = index pState + offset, enterCommand = Step }
  addInstruction i

addInstruction :: Instruction -> Run ()
addInstruction i = modify $ \pState -> pState { previous = i : previous pState }

runBack :: Instruction -> Run ()
runBack i = do
  previous <- gets previous
  case previous of
    [] -> liftIO (putStrLn "Can't go further back") *> execStepped i
    (p:ps) -> do
      offset <- back p
      modify $ \pState -> pState { previous = ps, index = index pState - offset, enterCommand = Back }
      if isJump p then runBack p -- Skip back through control flow
      else execStepped p
