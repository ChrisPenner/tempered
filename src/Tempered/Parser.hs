{-# language OverloadedStrings #-}
module Tempered.Parser
  ( templateFromFile
  , parseTemplate
  , handleTemplateError
  ) where

import Tempered.Template
import System.Exit

import Control.Applicative (liftA2)

import Text.Parsec
import Text.Parsec.String

infix 0 ?>
(?>) :: String -> ParsecT s u m a -> ParsecT s u m a
(?>) = flip (<?>)

templateFromFile :: FilePath -> IO (Either ParseError (Template Command))
templateFromFile fname = do
  file <- readFile fname
  return $ parseTemplate fname file

parseTemplate :: FilePath -> String -> Either ParseError (Template Command)
parseTemplate = runP templateP ()

handleTemplateError :: Either ParseError (Template a) -> IO (Template a)
handleTemplateError (Left err) = print err >> exitFailure
handleTemplateError (Right temp) = return temp

templateP :: Parser (Template Command)
templateP = "template" ?> do
  optional (try shebangP)
  contents <- many (cmd <|> txt)
  eof
  return $ Template contents
    where
      cmd = Right <$> commandP
      txt = Left <$> many1 (notFollowedBy (string "{{") *> anyChar)

shebangP :: Parser String
shebangP = "shebang" ?>
  liftA2 (++) (lookAhead (string "#!") *> string "#!") (manyTill anyChar (char '\n'))

commandP :: Parser Command
commandP = "command" ?> do
  _ <- string "{{"
  cmdString <- manyTill anyChar (string "}}")
  optional newline
  return $ Command cmdString
