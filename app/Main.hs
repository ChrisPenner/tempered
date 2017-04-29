{-# language OverloadedStrings #-}
module Main where

import System.Environment
import System.Directory
import System.Exit

import Paths_tempered (version)
import Data.Version (showVersion)

import Data.Foldable
import Data.Monoid
import qualified Data.Map as M
import Control.Monad.Reader

import Options.Applicative

import Tempered.Options
import Tempered.Parser
import Tempered.Template

data Options = Options
  { files :: [String]
  , versionFlag :: Bool
  } deriving (Show)


options :: Parser Options
options = Options <$> many (argument str meta) <*> switch vers
  where meta = help "The templates to interpolate" <> metavar "templates"
        vers = long "version" <> short 'v' <> help "Show version"

argParser :: ParserInfo Options
argParser = info (options <**> helper)
              ( fullDesc <>
                progDesc "Interpolate templates to stdout" <>
                header "Tempered - Templating engine based on shell interpolation")

-- | Run tempered on cmdline args.
main :: IO ()
main = do
  Options filenames versionF <- execParser argParser
  when versionF $ putStrLn (showVersion version) >> exitSuccess
  envVars <- getEnvVars
  templates <- getTemplates filenames
  runReaderT (renderOutput templates) envVars
  where
    -- Choose stdin if we're not given any files
    getTemplates [] = (:[]) <$> readStdInTemplate
    getTemplates filenames = traverse (templateFromFile >=> handleTemplateError) filenames

    readStdInTemplate = getContents >>= handleTemplateError . parseTemplate "stdin"
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
