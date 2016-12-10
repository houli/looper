module Command
  ( Command(..)
  , parseCommand
  ) where

import Control.Applicative ((<|>))
import Text.Trifecta

data Command = Inspect
             | InspectVariable String
             | Step
             | Location
             deriving (Eq, Read, Show)

parseCommand :: String -> Maybe Command
parseCommand str = case parseString (spaces *> command <* eof) mempty str of
                     Failure _       -> Nothing
                     Success command -> Just command

command :: Parser Command
command = try inspectVariable <|> inspect <|> step <|> location

inspectVariable :: Parser Command
inspectVariable = do
  symbol "inspect"
  name <- some letter
  pure $ InspectVariable name

inspect :: Parser Command
inspect = symbol "inspect" *> pure Inspect

step :: Parser Command
step = symbol "step" *> pure Step

location :: Parser Command
location = symbol "location" *> pure Location
