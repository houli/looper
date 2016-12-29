module Main where

import Control.Monad (unless)

import Interpreter
import Program
import StaticAnalysis

test2 :: Program
test2 = Assign "unused" (Const (I 3)) :>
        Assign "arg" (Const (I 10)) :>
        Assign "scratch" (Var "arg") :>
        Assign "total" (Const (I 1)) :>
        While (Gt (Var "scratch") (Const (I 1))) (
          Assign "total" (Mul (Var "total") (Var "scratch")) :>
          Assign "scratch" (Sub (Var "scratch") (Const (I 1))) :>
          Print (Var "scratch")
        ) :>
        Print (Var "total")

main :: IO ()
main = do
  let vars = unusedVariables test2
  unless (null vars) (mapM_ printUnused vars)
  run test2
    where printUnused var = putStrLn $ show var ++ " is assigned but not used"
