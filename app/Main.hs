module Main where

import System.Environment
import System.Directory

import Data.Foldable
import qualified Data.Map as M
import Control.Monad.Reader

import Tempered.Options
import Tempered.Parser
import Tempered.Template

-- | Run tempered on cmdline args.
main :: IO ()
main = do
  envVars <- getEnvVars
  filenames <- getArgs
  templates <- traverse (templateFromFile >=> handleTemplateError) filenames
  runReaderT (renderOutput templates) envVars
  where
    renderOutput = traverse_ (interpTemplate  >=> liftIO . putStr)

-- | Combine local and global environment variables
getEnvVars :: IO EnvVars
getEnvVars = do
  cwd <- getCurrentDirectory
  globalEnvVars <- getEnvironment
  localEnvVars <- getProjectOptions cwd
  envTemplates <- traverse (handleTemplateError . parseTemplate "env.yaml") localEnvVars
  interpolatedLocalEnvVars <- runReaderT (traverse interpTemplate envTemplates) mempty
  -- Precedence to local env vars; last in list has precedence
  return (globalEnvVars ++ M.toList interpolatedLocalEnvVars)
