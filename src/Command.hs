module Command
  ( Command(..)
  , parseCommand
  ) where

import Control.Applicative ((<|>))
import Text.Trifecta

data Command = Inspect
             | InspectVariable String
             | Step
             | Back
             | Location
             | Debug
             deriving (Eq, Read, Show)

parseCommand :: String -> Maybe Command
parseCommand str = case parseString (spaces *> command <* eof) mempty str of
                     Failure _       -> Nothing
                     Success command -> Just command

command :: Parser Command
command = try inspectVariable <|> inspect <|> step <|> back <|> location <|> debug

debug :: Parser Command
debug = symbol "d" *> pure Debug

anyInspect :: Parser String
anyInspect = try (symbol "i") <|> symbol "inspect"

inspectVariable :: Parser Command
inspectVariable = anyInspect *> (InspectVariable <$> token (some letter))

inspect :: Parser Command
inspect = anyInspect *> pure Inspect

step :: Parser Command
step = (symbol "step" <|> symbol "s") *> pure Step

back :: Parser Command
back = (symbol "back" <|> symbol "b") *> pure Back

location :: Parser Command
location = (symbol "location" <|> symbol "l") *> pure Location
