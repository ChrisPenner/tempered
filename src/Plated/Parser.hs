{-# language OverloadedStrings #-}
module Plated.Parser
  ( templateFromFile
  , parseTemplate
  , handleTemplateError
  ) where

import Plated.Template
import Plated.Command
import System.Exit

import Data.Text.IO as TIO
import qualified Data.Text as T

import Control.Applicative (liftA2)

import Text.Parsec
import Text.Parsec.Text


infix 0 ?>
(?>) :: String -> ParsecT s u m a -> ParsecT s u m a
(?>) = flip (<?>)

templateFromFile :: FilePath -> IO (Either ParseError (Template Command))
templateFromFile fname = do
  file <- TIO.readFile fname
  return $ parseTemplate fname file

parseTemplate :: FilePath -> T.Text -> Either ParseError (Template Command)
parseTemplate = runP templateParser ()

handleTemplateError :: Either ParseError (Template a) -> IO (Template a)
handleTemplateError (Left err) = print err >> exitFailure
handleTemplateError (Right temp) = return temp

templateParser :: Parser (Template Command)
templateParser = do
  optional shebang
  tmp <- template
  eof
  return tmp

template :: Parser (Template Command)
template = Template <$> many (cmd <|> txt)
    where
      cmd = Right <$> command
      txt = Left . T.pack <$> many1 (notFollowedBy (string "{{") *> anyChar)

shebang :: Parser String
shebang = "she-bang" ?> liftA2 (++) (string "#!") (manyTill anyChar (char '\n'))

command :: Parser Command
command = do
  _ <- string "{{"
  cmdString <- manyTill anyChar (string "}}")
  return $ Command (T.pack cmdString)
