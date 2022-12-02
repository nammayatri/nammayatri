module Main where

import "mock-idfy" App
import EulerHS.Prelude

main :: IO ()
main = runMockIdfy id
