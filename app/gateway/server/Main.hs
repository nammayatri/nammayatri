module Main where

import App (runGateway)
import EulerHS.Prelude

main :: IO ()
main = runGateway id
