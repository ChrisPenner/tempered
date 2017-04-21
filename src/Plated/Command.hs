module Plated.Command
  ( Command(..)
  , interpCommand
  ) where

import System.Process (readCreateProcessWithExitCode, shell)
import qualified Data.Text as T

data Command =
  Command T.Text
  deriving Show

interpCommand :: T.Text -> Command -> IO T.Text
interpCommand inp (Command cmd) = do
  (_, out, _) <- readCreateProcessWithExitCode (shell $ T.unpack cmd) (T.unpack inp)
  return . T.pack $ out


