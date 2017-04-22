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

-- | Parse a template from a file.
templateFromFile :: FilePath -> IO (Either ParseError (Template Command))
templateFromFile fname = do
  file <- readFile fname
  return $ parseTemplate fname file

-- | Parse a template from a string with a given filename for errors.
parseTemplate :: FilePath -> String -> Either ParseError (Template Command)
parseTemplate = runP templateP ()

-- | Fail if parsing errors occurred, otherwise return the template.
handleTemplateError :: Either ParseError (Template a) -> IO (Template a)
handleTemplateError (Left err) = print err >> exitFailure
handleTemplateError (Right temp) = return temp

-- | Template Parser
templateP :: Parser (Template Command)
templateP = "template" ?> do
  optional (try shebangP)
  contents <- many (cmd <|> txt)
  eof
  return $ Template contents
    where
      cmd = Right <$> commandP
      txt = Left <$> many1 (notFollowedBy (string "{{") *> anyChar)

-- | Shebang Parser
shebangP :: Parser String
shebangP = "shebang" ?>
  liftA2 (++) (lookAhead (string "#!") *> string "#!") (manyTill anyChar (char '\n'))

-- | Command Parser
commandP :: Parser Command
commandP = "command" ?> do
  _ <- string "{{"
  cmdString <- manyTill anyChar (string "}}")
  optional newline
  return $ Command cmdString
