module Main where

import "mock-public-transport-bpp" App
import Kernel.Prelude

main :: IO ()
main = runMock identity
