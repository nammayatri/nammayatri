module Main where

import App (runMockFcm)
import EulerHS.Prelude

main :: IO ()
main = runMockFcm id
