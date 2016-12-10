module Main where

import qualified Data.Map as Map

import Eval

main :: IO ()
main = case runEval Map.empty (eval (Const (I 3))) of
  Left err -> print err
  Right val -> print val
