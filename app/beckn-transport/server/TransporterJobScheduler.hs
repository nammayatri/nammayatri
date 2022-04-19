module Main where

import App.SchedulerExample
import Beckn.Prelude

main :: IO ()
main = runTransporterScheduler identity
