module Main where

import Beckn.Utils.Logging
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R

main :: IO ()
main = pure ()

runWithFlowRuntime :: L.Flow a -> IO a
runWithFlowRuntime flow = do
  let logRuntime =
        getEulerLoggerRuntime $
          LoggerConfig
            { isAsync = True,
              level = DEBUG,
              logToFile = False,
              logFilePath = "",
              logToConsole = False,
              logRawSql = False
            }
  R.withFlowRuntime (Just logRuntime) $ \flowRt -> I.runFlow flowRt flow
