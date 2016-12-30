module Eval
  ( Env
  , Expr(..)
  , Name
  , Val(..)
  , eval
  , runEval
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Char (toLower)
import qualified Data.Map as Map
import           Prelude hiding (lookup)

data Val = I Int
         | B Bool
         deriving (Eq, Read)

instance Show Val where
  show (I i) = show i
  show (B b) = toLower <$> show b

data Expr = Const Val
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Eq Expr Expr
          | Gt Expr Expr
          | Lt Expr Expr
          | Var Name
          deriving (Eq, Read)

instance Show Expr where
  show (Const val) = show val
  show (Add e0 e1) = "(" ++ show e0 ++ " + " ++ show e1 ++ ")"
  show (Sub e0 e1) = "(" ++ show e0 ++ " - " ++ show e1 ++ ")"
  show (Mul e0 e1) = "(" ++ show e0 ++ " * " ++ show e1 ++ ")"
  show (Div e0 e1) = "(" ++ show e0 ++ " / " ++ show e1 ++ ")"
  show (And e0 e1) = "(" ++ show e0 ++ " && " ++ show e1 ++ ")"
  show (Or e0 e1) = "(" ++ show e0 ++ "||" ++ show e1 ++ ")"
  show (Not e) = "!" ++ show e
  show (Eq e0 e1) = "(" ++ show e0 ++ " == " ++ show e1 ++ ")"
  show (Gt e0 e1) = "(" ++ show e0 ++ " > " ++ show e1 ++ ")"
  show (Lt e0 e1) = "(" ++ show e0 ++ " < " ++ show e1 ++ ")"
  show (Var name) = name

type Name = String
type Env = Map.Map Name [Val]
type Eval a = ReaderT Env (Either String) a

runEval :: Env -> Eval a -> Either String a
runEval env ex = runReaderT ex env

lookup :: Name -> Env -> Eval Val
lookup k t = case Map.lookup k t of
               Just (val:vals) -> pure val
               _ -> fail ("Unknown variable " ++ k)

evali :: (Int -> Int -> Int) -> Expr -> Expr -> Eval Val
evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                      (I i0, I i1) -> pure $ I (i0 `op` i1)
                      _            -> fail "type error in arithmetic expression"

evalb :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Eval Val
evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                      (B i0, B i1) -> pure $ B (i0 `op` i1)
                      _            -> fail "type error in boolean expression"

evalib :: (Int -> Int -> Bool) -> Expr -> Expr -> Eval Val
evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                       (I i0, I i1) -> pure $ B (i0 `op` i1)
                       _            -> fail "type error in arithmetic expression"

eval :: Expr -> Eval Val
eval (Const v) = pure v
eval (Add e0 e1) = evali (+) e0 e1
eval (Sub e0 e1) = evali (-) e0 e1
eval (Mul e0 e1) = evali (*) e0 e1
eval (Div e0 e1) = evali div e0 e1
eval (And e0 e1) = evalb (&&) e0 e1
eval (Or e0 e1) = evalb (||) e0 e1
eval (Not e0) = evalb (const not) e0 (Const (B True))
eval (Eq e0 e1) = evalib (==) e0 e1
eval (Gt e0 e1) = evalib (>) e0 e1
eval (Lt e0 e1) = evalib (<) e0 e1
eval (Var s) = ask >>= lookup s
