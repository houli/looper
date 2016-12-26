module Main where

import qualified Data.Map as Map

import Program
import StaticAnalysis

testProgram :: Program
testProgram = Assign "a" (Const (I 3)) :> Print (Var "a")

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
  case unusedVariables test2 of
    [] -> pure ()
    vars -> mapM_ printUnused vars
  run test2
    where printUnused var = putStrLn $ show var ++ " is assigned but not used"
