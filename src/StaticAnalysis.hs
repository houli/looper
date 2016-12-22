module StaticAnalysis
  ( unusedVariables
  ) where

import Data.List (nub, (\\))

import Eval
import Program

unusedVariables :: Program -> [String]
unusedVariables p = defines p \\ uses p

defines :: Program -> [String]
defines = nub . go
  where go (Assign s _) = [s]
        go (If _ p0 p1) = go p0 ++ go p1
        go (While _ p) = go p
        go (Print _) = []
        go (p0 :> p1) = go p0 ++ go p1
        go (Try p0 p1) = go p0 ++ go p1

uses :: Program -> [String]
uses = nub . go
  where go (Assign _ e) = usesExpr e
        go (If cond p0 p1) = usesExpr cond ++ go p0 ++ go p1
        go (While cond p) = usesExpr cond ++ go p
        go (Print e) = usesExpr e
        go (p0 :> p1) = go p0 ++ go p1
        go (Try p0 p1) = go p0 ++ go p1

usesExpr :: Expr -> [String]
usesExpr = nub . go
  where go (Const _) = []
        go (Add e0 e1) = go e0 ++ go e1
        go (Sub e0 e1) = go e0 ++ go e1
        go (Mul e0 e1) = go e0 ++ go e1
        go (Div e0 e1) = go e0 ++ go e1
        go (And e0 e1) = go e0 ++ go e1
        go (Or e0 e1) = go e0 ++ go e1
        go (Not e) = go e
        go (Eq e0 e1) = go e0 ++ go e1
        go (Gt e0 e1) = go e0 ++ go e1
        go (Lt e0 e1) = go e0 ++ go e1
        go (Var s) = [s]
