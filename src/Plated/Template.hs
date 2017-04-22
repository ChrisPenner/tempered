{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Plated.Template
  ( Template(..)
  , processTemplate
  ) where

import Control.Monad.Reader
import Data.Foldable

import Plated.Command
import Plated.Yaml

import qualified Data.Text as T

data Template a =
  Template [Either T.Text a]
  deriving Show

processTemplate :: (MonadReader EnvVars m, MonadIO m) => Template Command -> m T.Text
processTemplate (Template elems) = fold <$> mapM toText elems
    where
      toText = either return interpCommand
