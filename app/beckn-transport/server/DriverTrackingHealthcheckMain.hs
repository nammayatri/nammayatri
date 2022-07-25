module Main where

import DriverTrackingHealthCheck.App
import Prelude

main :: IO ()
main = runDriverHealthcheck id
