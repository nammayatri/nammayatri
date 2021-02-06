module Main where

import App
import BackgroundTaskManager
import EulerHS.Prelude
import System.Environment

main :: IO ()
main =
  lookupEnv "APP_MODE" >>= \case
    Just "API" -> runTransporterBackendApp id
    Just "BACKGROUND_TASK_MANAGER" -> runBackgroundTaskManager id
    _ -> runTransporterBackendApp id -- error out?
