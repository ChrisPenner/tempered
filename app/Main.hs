module Main where

import System.Environment
import System.Directory

import Data.Foldable
import Control.Monad.Reader

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Plated.Yaml
import Plated.Parser
import Plated.Template

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  options <- getProjectOptions cwd
  envTemplates <- traverse (handleTemplateError . parseTemplate "env.yaml" . T.pack) (_env options)
  envVars <- flip runReaderT mempty $ traverse processTemplate envTemplates
  filenames <- getArgs
  templates <- traverse templateFromFile filenames >>= traverse handleTemplateError
  flip runReaderT (T.unpack <$> envVars) $ processTemplates templates
  where
    processTemplates = traverse_ (processTemplate >=> liftIO . TIO.putStr)
