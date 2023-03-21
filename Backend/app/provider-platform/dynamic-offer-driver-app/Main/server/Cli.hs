module Main where

import qualified CliApp
import Prelude

main :: IO ()
main = CliApp.main =<< getArgs
