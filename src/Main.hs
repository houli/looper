module Main where

import Options.Applicative

import Interpreter
import Program
import StaticAnalysis

main :: IO ()
main = do
  fileName <- execParser opts
  contents <- readFile fileName
  let program = read contents
  mapM_ printUnused (unusedVariables program)
  run program
    where printUnused var = putStrLn $ show var ++ " is assigned but never used"
          parser = strOption (long "program" <> short 'p' <> metavar "PROGRAM")
          opts = info parser mempty
