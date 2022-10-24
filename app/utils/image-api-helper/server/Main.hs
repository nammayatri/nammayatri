module Main where

import "image-api-helper" App
import Beckn.Prelude

main :: IO ()
main = runMock identity
