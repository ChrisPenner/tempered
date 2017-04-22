{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
module Plated.Yaml
  ( PlatedOptions(..)
  , getProjectOptions
  , EnvVars
  ) where

import Control.Monad

import Data.Yaml as Y
import Data.Aeson.TH
import qualified Data.Map as M
import Data.Maybe
import Data.Default
import Data.List


import System.FilePath
import System.Directory
import System.Exit

type EnvVars = M.Map String String
data PlatedOptions = PlatedOptions
  { _env :: EnvVars
  , _output :: String
  } deriving Show

instance Default PlatedOptions where
  def = PlatedOptions M.empty "./"

-- Derive toJSON, fromJSON
$(deriveJSON defaultOptions{fieldLabelModifier=drop 1} ''PlatedOptions)

getProjectOptions :: FilePath -> IO PlatedOptions
getProjectOptions path = do
  mProjSettingsFile <- findProjSettings path
  mOptions <- traverse optionsFromFilename mProjSettingsFile
  return $ fromMaybe def mOptions

-- Retrieve an options object from a yaml file
optionsFromFilename :: FilePath -> IO PlatedOptions
optionsFromFilename = Y.decodeFileEither >=>
  \case
    Left err -> die . prettyPrintParseException $ err
    Right options -> return options

findProjSettings :: FilePath -> IO (Maybe FilePath)
findProjSettings fpath = do
  absPath <- makeAbsolute fpath
  let searchPaths = (</> "env.yaml") <$> recurseUp absPath
  listToMaybe <$> filterM doesFileExist searchPaths

recurseUp :: FilePath -> [FilePath]
recurseUp = unfoldr go
  where
    go "/" =  Nothing
    go path = Just (takeDirectory path, takeDirectory path)
