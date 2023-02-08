module Main where

import "image-api-helper" App
import Kernel.Prelude

main :: IO ()
main = runMock identity
