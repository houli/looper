module Main where

import Control.Monad (unless)
import Options.Applicative

import Interpreter
import Program
import StaticAnalysis

type Options = String

main :: IO ()
main = do
  fileName <- execParser opts
  contents <- readFile fileName
  let program = read contents :: Program
  let vars = unusedVariables program
  unless (null vars) (mapM_ printUnused vars)
  run program
    where printUnused var = putStrLn $ show var ++ " is assigned but not used"
          parser = strOption (long "program" <> short 'p' <> metavar "PROGRAM")
          opts = info parser mempty
