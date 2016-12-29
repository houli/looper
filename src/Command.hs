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
             deriving (Eq, Read, Show)

parseCommand :: String -> Maybe Command
parseCommand str = case parseString (spaces *> command <* eof) mempty str of
                     Failure _       -> Nothing
                     Success command -> Just command

command :: Parser Command
command = try inspectVariable
      <|> inspect
      <|> step
      <|> back
      <|> location

firstLetterOnly :: String -> Parser String
firstLetterOnly (c:cs) = symbol (c:cs) <|> symbol [c]
firstLetterOnly "" = symbol ""

inspectVariable :: Parser Command
inspectVariable = firstLetterOnly "inspect" *> (InspectVariable <$> token (some letter))

inspect :: Parser Command
inspect = firstLetterOnly "inspect" *> pure Inspect

step :: Parser Command
step = firstLetterOnly "step" *> pure Step

back :: Parser Command
back = firstLetterOnly "back" *> pure Back

location :: Parser Command
location = firstLetterOnly "location" *> pure Location
