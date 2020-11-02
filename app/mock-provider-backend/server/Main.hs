module Main where

import "mock-provider-backend" App (runMockProvider)
import EulerHS.Prelude

main :: IO ()
main = runMockProvider id
