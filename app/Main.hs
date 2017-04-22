module Main where

import System.Environment
import System.Directory

import Data.Foldable
import Control.Monad.Reader

import Plated.Options
import Plated.Parser
import Plated.Template

main :: IO ()
main = do
  envVars <- getEnvVars
  filenames <- getArgs
  templates <- traverse (templateFromFile >=> handleTemplateError) filenames
  runReaderT (renderOutput templates) (Just envVars)
  where
    renderOutput = traverse_ (interpTemplate  >=> liftIO . putStr)

getEnvVars :: IO EnvVars
getEnvVars = do
  cwd <- getCurrentDirectory
  envVars <- getProjectOptions cwd
  envTemplates <- traverse (handleTemplateError . parseTemplate "env.yaml") envVars
  runReaderT (traverse interpTemplate envTemplates) mempty
