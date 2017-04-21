{-# language OverloadedStrings #-}
module Plated.Parser
  ( templateFromFile
  , interpolate
  , shebang
  ) where

import Plated.Template
import Plated.Command

import Data.Text.IO as TIO
import qualified Data.Text as T

import Control.Applicative (liftA2)

import Text.Parsec
import Text.Parsec.Text


infix 0 ?>
(?>) :: String -> ParsecT s u m a -> ParsecT s u m a
(?>) = flip (<?>)

templateFromFile :: FilePath -> IO (Either ParseError (Template Directive))
templateFromFile fname = do
  file <- TIO.readFile fname
  return $ runP templateParser () fname file

templateParser :: Parser (Template Directive)
templateParser = do
  optional shebang
  tmp <- template
  eof
  return tmp

template :: Parser (Template Directive)
template = Template <$> many (dir <|> txt)
    where
      dir = Right <$> directive
      txt = Left . T.pack <$> many1 (notFollowedBy (string "{{") *> anyChar)

shebang :: Parser String
shebang = "she-bang" ?> liftA2 (++) (string "#!") (manyTill anyChar (char '\n'))

directive :: Parser Directive
directive = "Directive" ?> do
  spaces
  _ <- string "{{"
  spaces
  mCommand <- optionMaybe command
  txt <- manyTill anyChar (string "}}")
  optional newline
  return $ Directive mCommand (T.pack txt)

command :: Parser Command
command = do
  _ <- string "[["
  cmdString <- manyTill anyChar (string "]]")
  spaces
  return $ Command (T.pack cmdString)

interpolate :: Template Directive -> T.Text
interpolate = const "hi"
