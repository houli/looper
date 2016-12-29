module Main where

import Interpreter
import Program
import StaticAnalysis

testProgram :: Program
testProgram = Assign "a" (Const (I 3)) :> Print (Var "a")

testIf :: Program
testIf = Assign "i" (Const (I 3)) :>
         If (Gt (Var "i") (Const (I 2))) (
           Assign "i" (Sub (Var "i") (Const (I 1))) :>
           Print (Var "i")
         ) (
           Assign "i" (Const (I 10)) :>
           Print (Var "i")
         ) :>
         Assign "i" (Const (I 20)) :>
         Print (Var "i")

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
