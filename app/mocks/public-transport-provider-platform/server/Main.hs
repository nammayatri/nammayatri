module Main where

import "mock-public-transport-provider-platform" App
import Kernel.Prelude

main :: IO ()
main = runMock identity
