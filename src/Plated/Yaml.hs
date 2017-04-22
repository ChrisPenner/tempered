{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
module Plated.Yaml
  ( PlatedOptions(..)
  , optionsFromFilename
  , recurseUp
  ) where

import Control.Monad

import Data.Yaml as Y
import Data.Aeson.TH
import Data.Map as M
import Data.Default
import Data.List


import System.FilePath
-- import System.Directory
import System.Exit

data PlatedOptions = PlatedOptions
  { _env :: Map String String
  , _output :: String
  } deriving Show

instance Default PlatedOptions where
  def = PlatedOptions M.empty "./"

-- Derive toJSON, fromJSON
$(deriveJSON defaultOptions{fieldLabelModifier=drop 1} ''PlatedOptions)

-- Retrieve an options object from a yaml file
optionsFromFilename :: FilePath -> IO PlatedOptions
optionsFromFilename = Y.decodeFileEither >=>
  \case
    Left err -> die . prettyPrintParseException $ err
    Right options -> return options

-- findProjRootFrom :: FilePath -> IO (Maybe FilePath)
-- findProjRootFrom (makeAbsolute -> fpath) = return . pure $ fpath

recurseUp :: FilePath -> [FilePath]
recurseUp = unfoldr go
  where
    go "/" =  Nothing
    go path = Just (takeDirectory path, takeDirectory path)
