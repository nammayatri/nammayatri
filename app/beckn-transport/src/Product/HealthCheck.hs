module Product.HealthCheck where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Utils.IOLogging (LoggerEnv)
import EulerHS.Prelude
import Servant (Get, JSON)
import Types.Error
import Types.Metrics (CoreMetricsContainer)
import Utils.Common

type HealthCheckAPI = Get '[JSON] Text

healthCheckAPI :: Proxy HealthCheckAPI
healthCheckAPI = Proxy

healthCheck ::
  ( HasField "coreMetrics" r CoreMetricsContainer,
    HasField "isShuttingDown" r Shutdown,
    HasField "loggerEnv" r LoggerEnv
  ) =>
  Text ->
  FlowHandlerR r Text
healthCheck serviceName = withFlowHandlerAPI do
  mbTime <- Redis.getKeyRedis (mkKey serviceName)
  maybe markAsDead checkLastUpdateTime mbTime
  where
    markAsDead = throwError ServiceUnavailable
    checkLastUpdateTime lastUpdateTime = do
      now <- getCurrentTime
      let diffTime = diffUTCTime now lastUpdateTime
      if diffTime > 10
        then markAsDead
        else return "Service is up!"

mkKey :: Text -> Text
mkKey serviceName = "beckn:" <> serviceName <> ":service"

iAmAlive :: MonadFlow m => Text -> m ()
iAmAlive serviceName = getCurrentTime >>= Redis.setKeyRedis (mkKey serviceName)
