{-# language OverloadedStrings #-}
module Plated.Template
  ( Template(..)
  , processTemplate
  ) where

import Plated.Command
import Data.Foldable

import qualified Data.Text as T

data Template a =
  Template [Either T.Text a]
  deriving Show

processTemplate :: Template Command -> IO T.Text
processTemplate (Template elems) = fold <$> mapM (either return interpCommand) elems
