module Main where

import System.Environment
import System.Exit
import Data.Foldable
import Data.Either
import Control.Monad

import qualified Data.Text.IO as TIO

import Plated.Parser
import Plated.Template

main :: IO ()
main = do
  filenames <- getArgs
  templates <- traverse templateFromFile filenames
  when (not . null $ lefts templates) $
    print (lefts templates) >> exitFailure
  traverse_ (processTemplate >=> TIO.putStr) (rights templates)
