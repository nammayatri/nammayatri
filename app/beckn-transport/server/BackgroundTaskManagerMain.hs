module Main where

import BackgroundTaskManager
import EulerHS.Prelude
import System.Environment

main :: IO ()
main = runBackgroundTaskManager id
