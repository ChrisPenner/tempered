{-# language FlexibleContexts #-}
module Plated.Command
  ( Command(..)
  , interpCommand
  ) where

import System.Process (readCreateProcessWithExitCode, shell, env)
import qualified Data.Text as T
import Control.Monad.Reader
import qualified Data.Map as M

import Plated.Yaml

data Command =
  Command T.Text
  deriving Show

interpCommand :: (MonadReader EnvVars m, MonadIO m) => Command -> m T.Text
interpCommand (Command cmd) = do
  env' <- ask
  let process = (shell $ T.unpack cmd){env=Just (M.toList env')}
  (_, out, _) <- liftIO $ readCreateProcessWithExitCode process ""
  return . T.pack $ out


