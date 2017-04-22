module Plated.Command
  ( Command(..)
  , interpCommand
  ) where

import System.Process (readCreateProcessWithExitCode, shell)
import qualified Data.Text as T

data Command =
  Command T.Text
  deriving Show

interpCommand :: Command -> IO T.Text
interpCommand (Command cmd) = do
  (_, out, _) <- readCreateProcessWithExitCode (shell $ T.unpack cmd) ""
  return . T.pack $ out


