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
    HasField "hedisEnv" r Redis.HedisEnv
  ) =>
  FlowHandlerR r Text
healthCheck = withFlowHandlerAPI do
  mbTime <- Redis.get key
  maybe markAsDead checkLastUpdateTime mbTime
  where
    markAsDead = throwError ServiceUnavailable
    checkLastUpdateTime lastUpdateTime = do
      now <- getCurrentTime
      let diffTime = diffUTCTime now lastUpdateTime
      if diffTime > 10
        then markAsDead
        else return "Service is up!"

key :: Text
key = "beckn:driver-tracking-healthcheck:service"

--TODO: Make ServiceHealthChecker util in shared-kernel
iAmAlive :: (Redis.HedisFlow m r, MonadTime m) => m ()
iAmAlive = getCurrentTime >>= Redis.set key
