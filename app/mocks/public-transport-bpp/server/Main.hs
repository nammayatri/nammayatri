module Main where

import "mock-public-transport-bpp" App
import Beckn.Prelude

main :: IO ()
main = runMock identity
