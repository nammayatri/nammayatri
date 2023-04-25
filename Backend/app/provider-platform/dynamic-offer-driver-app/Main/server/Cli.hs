module Main where

import qualified CliApp
import System.Environment
import Prelude

main :: IO ()
main = CliApp.main =<< getArgs
