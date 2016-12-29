module Program
  ( module Eval
  , Program(..)
  ) where

import Eval (Expr(..), Name, Val(..))

data Program = Assign Name Expr
             | If Expr Program Program
             | While Expr Program
             | Print Expr
             | Program :> Program -- Sequencing operator
             deriving (Eq, Read, Show)
