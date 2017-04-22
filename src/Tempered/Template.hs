{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Tempered.Template
  ( Template(..)
  , Command(..)
  , interpTemplate
  ) where

import System.Process
import Control.Monad.Reader

import Data.Foldable

import Tempered.Options

data Template a =
  Template [Either String a]
  deriving Show

interpTemplate :: (MonadReader EnvVars m, MonadIO m) => Template Command -> m String
interpTemplate (Template elems) = fold <$> mapM toText elems
    where
      toText = either return interpCommand

data Command =
  Command String
  deriving Show

interpCommand :: (MonadReader EnvVars m, MonadIO m) => Command -> m String
interpCommand (Command cmd) = do
  envVars <- ask
  let process = (shell cmd){env=Just envVars}
  liftIO $ readCreateProcess process ""
