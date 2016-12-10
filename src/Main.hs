module Main where

import qualified Data.Map as Map

import Eval
import Program

testExpr :: Expr
testExpr = Const (I 3)

testProgram :: Program
testProgram = Assign "a" (Const (I 3)) :> Print (Var "a")

main :: IO ()
main = do
  run testProgram
  case runEval Map.empty (eval testExpr) of
    Left err -> print err
    Right val -> print val
