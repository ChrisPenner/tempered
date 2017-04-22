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

-- | Represents values interspersed with text.
data Template a =
  Template [Either String a]
  deriving Show

-- | Given an execution environment render a template into a string.
interpTemplate :: (MonadReader EnvVars m, MonadIO m) => Template Command -> m String
interpTemplate (Template elems) = fold <$> mapM toText elems
    where
      toText = either return interpCommand

-- | Represents a command to be run by the system.
data Command =
  Command String
  deriving Show

-- | Run a command in an environment returning the result.
interpCommand :: (MonadReader EnvVars m, MonadIO m) => Command -> m String
interpCommand (Command cmd) = do
  envVars <- ask
  let process = (shell cmd){env=Just envVars}
  liftIO $ readCreateProcess process ""
