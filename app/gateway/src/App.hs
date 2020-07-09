module App
  ( runGateway,
  )
where

import EulerHS.Prelude
import EulerHS.Runtime as E
import EulerHS.Types as E
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )
import System.Environment (lookupEnv)

runGateway :: IO ()
runGateway = do
  port <- fromMaybe 8015 . (>>= readMaybe) <$> lookupEnv "PORT"
  let loggerCfg =
        E.defaultLoggerConfig
          { E._logToFile = True,
            E._logFilePath = "/tmp/beckn-gateway.log",
            E._isAsync = True
          }
  E.withFlowRuntime (Just loggerCfg) $ \flowRt ->
    pure ()
