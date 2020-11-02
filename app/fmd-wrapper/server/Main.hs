module Main where

import App (runFMDWrapper)
import EulerHS.Prelude

main :: IO ()
main = runFMDWrapper id
