{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Plated.Template
  ( Template(..)
  , Command(..)
  , interpTemplate
  ) where

import System.Process
import Control.Monad.Reader

import Data.Foldable
import qualified Data.Map as M

import Plated.Options

data Template a =
  Template [Either String a]
  deriving Show

interpTemplate :: (MonadReader (Maybe EnvVars) m, MonadIO m) => Template Command -> m String
interpTemplate (Template elems) = fold <$> mapM toText elems
    where
      toText = either return interpCommand

data Command =
  Command String
  deriving Show

interpCommand :: (MonadReader (Maybe EnvVars) m, MonadIO m) => Command -> m String
interpCommand (Command cmd) = do
  envVars <- ask
  let process = (shell cmd){env=M.toList <$> envVars}
  liftIO $ readCreateProcess process ""
