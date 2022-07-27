module Main where

import App.Scheduler
import Beckn.Prelude

main :: IO ()
main = runTransporterScheduler identity identity
