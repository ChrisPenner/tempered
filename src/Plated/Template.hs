{-# language OverloadedStrings #-}
module Plated.Template
  ( Template(..)
  , Directive(..)
  , processTemplate
  ) where

import Plated.Command
import Data.Foldable

import qualified Data.Text as T

data Template a =
  Template [Either T.Text a]
  deriving Show

data Directive =
  Directive (Maybe Command) T.Text
  deriving Show

processTemplate :: Template Directive -> IO T.Text
processTemplate (Template elems) = fold <$> mapM (either return fromDirective) elems
  where
    fromDirective (Directive mCmd txt) = maybe (return "") (interpCommand txt) mCmd
