module Main where

import "mock-dunzo" App (runService)
import Beckn.Prelude

main :: IO ()
main = runService identity
