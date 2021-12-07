module Main where

import App (runRegistryService)
import Prelude

main :: IO ()
main = runRegistryService id
