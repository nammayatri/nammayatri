module App.DriverTrackingHealthcheck.Config where

import Beckn.External.Encryption (EncTools)
import Beckn.Storage.Esqueleto.Config (EsqDBConfig)
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.Client (HttpClientOptions)
import EulerHS.Prelude
import EulerHS.Types (RedisConfig)

data AppCfg = AppCfg
  { loggerConfig :: LoggerConfig,
    metricsPort :: Int,
    healthcheckPort :: Int,
    httpClientOptions :: HttpClientOptions,
    graceTerminationPeriod :: Seconds,
    redisCfg :: RedisConfig,
    esqDBCfg :: EsqDBConfig,
    nwAddress :: BaseUrl,
    fcmJsonPath :: Maybe Text,
    fcmUrl :: BaseUrl,
    encTools :: EncTools,
    driverAllowedDelay :: Seconds,
    notificationMinDelay :: Microseconds
  }
  deriving (Generic, FromDhall)
