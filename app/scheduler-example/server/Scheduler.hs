module Main where

import App.Scheduler
import Kernel.Prelude

main :: IO ()
main = runExampleScheduler identity
