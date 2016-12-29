module Instruction
  ( Instruction(..)
  , fromProgram
  ) where

import           Program (Expr, Name, Program)
import qualified Program as P

data Instruction = Assign Name Expr
                 | JMP Int
                 | JMPF Expr Int
                 | Print Expr
                 deriving (Eq, Show)

fromProgram :: Program -> [Instruction]
fromProgram (P.Assign s e) = [Assign s e]
fromProgram (P.If cond p0 p1) = translateIf cond p0 p1
fromProgram (P.While cond p) = translateWhile cond p
fromProgram (P.Print e) = [Print e]
fromProgram (s0 P.:> s1) = fromProgram s0 ++ fromProgram s1

translateIf :: Expr -> Program -> Program -> [Instruction]
translateIf cond p0 p1 = (JMPF cond elseOffset : iif) ++ (JMP exitOffset : eelse)
  where iif = fromProgram p0
        eelse = fromProgram p1
        elseOffset = length iif + 2
        exitOffset = length eelse + 1

translateWhile :: Expr -> Program -> [Instruction]
translateWhile cond p = (JMPF cond exitOffset : while) ++ [JMP backOffset]
  where while = fromProgram p
        exitOffset = length while + 2
        backOffset = -(length while + 1)
