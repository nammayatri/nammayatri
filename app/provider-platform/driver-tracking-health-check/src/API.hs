module API (healthCheckAPI, healthCheck, iAmAlive) where

import EulerHS.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (LoggerEnv)
import Servant (Get, JSON)
import Tools.Error
import Tools.Metrics (CoreMetricsContainer)

type HealthCheckAPI = Get '[JSON] Text

healthCheckAPI :: Proxy HealthCheckAPI
healthCheckAPI = Proxy

healthCheck ::
  ( HasField "coreMetrics" r CoreMetricsContainer,
    HasField "isShuttingDown" r Shutdown,
    HasField "loggerEnv" r LoggerEnv,
    HasField "hedisEnv" r Redis.HedisEnv,
    HasField "driverAppName" r Text
  ) =>
  FlowHandlerR r Text
healthCheck = withFlowHandlerAPI do
  redisPrefix <- asks (.driverAppName)
  mbTime <- Redis.get $ key redisPrefix
  maybe markAsDead checkLastUpdateTime mbTime
  where
    markAsDead = throwError ServiceUnavailable
    checkLastUpdateTime lastUpdateTime = do
      now <- getCurrentTime
      let diffTime = diffUTCTime now lastUpdateTime
      if diffTime > 10
        then markAsDead
        else return "Service is up!"

key :: Text -> Text
key prefix = prefix <> "driver-tracking-healthcheck:service"

--TODO: Make ServiceHealthChecker util in shared-kernel
iAmAlive :: (Redis.HedisFlow m r, HasField "driverAppName" r Text, MonadTime m) => m ()
iAmAlive = do
  redisPrefix <- asks (.driverAppName)
  getCurrentTime >>= Redis.set (key redisPrefix)
