{-# language LambdaCase #-}
module Plated.Options
  ( EnvVars
  , getProjectOptions
  ) where

import Control.Monad

import Data.Yaml as Y
import qualified Data.Map as M
import Data.Maybe
import Data.List

import System.FilePath
import System.Directory
import System.Exit

type EnvVars = M.Map String String

getProjectOptions :: FilePath -> IO EnvVars
getProjectOptions path = do
  mProjSettingsFile <- findProjSettings path
  mOptions <- traverse optionsFromFilename mProjSettingsFile
  return $ fromMaybe mempty mOptions

-- Retrieve an options object from a yaml file
optionsFromFilename :: FilePath -> IO EnvVars
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
