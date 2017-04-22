{-# language LambdaCase #-}
module Tempered.Options
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

type EnvVars = [(String, String)]
type EnvMap = M.Map String String

-- | Given a directory tries to find env.yaml recursively upwards;
-- parses `EnvMap` from the file if found.
getProjectOptions :: FilePath -> IO EnvMap
getProjectOptions path = do
  mProjSettingsFile <- findProjSettings path
  mOptions <- traverse optionsFromFilename mProjSettingsFile
  return $ fromMaybe mempty mOptions

-- Retrieve an EnvMap from a yaml file
optionsFromFilename :: FilePath -> IO EnvMap
optionsFromFilename = Y.decodeFileEither >=>
  \case
    Left err -> die . prettyPrintParseException $ err
    Right options -> return options

-- Try to find an 'env.yaml' file.
findProjSettings :: FilePath -> IO (Maybe FilePath)
findProjSettings fpath = do
  absPath <- makeAbsolute fpath
  let searchPaths = (</> "env.yaml") <$> recurseUp absPath
  listToMaybe <$> filterM doesFileExist searchPaths

-- Get all parent directories of a directory path.
recurseUp :: FilePath -> [FilePath]
recurseUp = unfoldr go
  where
    go "/" =  Nothing
    go path = Just (path, takeDirectory path)
