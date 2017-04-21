module Plated.Template
  ( Template(..)
  , Directive(..)
  , processTemplate
  ) where

import Plated.Command

import qualified Data.Text as T

data Template a =
  Template [Either T.Text a]
  deriving Show

data Directive =
  Directive (Maybe Command) T.Text
  deriving Show

processTemplate :: Template Directive -> T.Text
processTemplate (Template elems) = foldMap (either id fromDirective) elems
  where
    fromDirective (Directive _ txt) = txt
