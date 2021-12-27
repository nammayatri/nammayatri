module Main where

import App (runService)
import Beckn.Prelude

main :: IO ()
main = runService identity
