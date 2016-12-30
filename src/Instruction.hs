module Instruction
  ( Instruction(..)
  , Tape
  , fromProgram
  , isJump
  ) where

import           Program (Expr, Name, Program)
import qualified Program as P

data Instruction = Assign Name Expr
                 | JMP Int
                 | JMPF Expr Int
                 | Print Expr
                 deriving (Eq, Show)

type Tape = [Instruction]

-- Turn a program tree into a list of instructions
fromProgram :: Program -> Tape
fromProgram (P.Assign s e) = [Assign s e]
fromProgram (P.If cond p0 p1) = translateIf cond p0 p1
fromProgram (P.While cond p) = translateWhile cond p
fromProgram (P.Print e) = [Print e]
fromProgram (s0 P.:> s1) = fromProgram s0 ++ fromProgram s1

translateIf :: Expr -> Program -> Program -> Tape
translateIf cond p0 p1 = (JMPF cond elseOffset : iif) ++ (JMP exitOffset : eelse)
  where iif = fromProgram p0
        eelse = fromProgram p1
        elseOffset = length iif + 2 -- Where the else code begins
        exitOffset = length eelse + 1 -- Exit the if block

translateWhile :: Expr -> Program -> Tape
translateWhile cond p = (JMPF cond exitOffset : while) ++ [JMP backOffset]
  where while = fromProgram p
        exitOffset = length while + 2 -- Where the program continues after the while
        backOffset = -(length while + 1) -- Jump back to the condition check

isJump :: Instruction -> Bool
isJump JMP{} = True
isJump JMPF{} = True
isJump _ = False
