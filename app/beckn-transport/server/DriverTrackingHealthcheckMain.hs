module Main where

import App.DriverTrackingHealthcheck
import Prelude

main :: IO ()
main = runDriverHealthcheck id
