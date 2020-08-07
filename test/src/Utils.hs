module Utils where

import EulerHS.Prelude
import qualified EulerHS.Types as T
import Servant.Client

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient clientEnv x = runClientM x clientEnv

getLoggerCfg :: String -> T.LoggerConfig
getLoggerCfg t =
  T.defaultLoggerConfig
    { T._logToFile = True,
      T._logFilePath = "/tmp/log-" <> t,
      T._isAsync = False
    }
