{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE RankNTypes #-}

module App (startProducer) where

import Data.Function hiding (id)
import qualified Database.Redis as Redis
import Debug.Trace as T
import Environment
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.KVConnector.Metrics as KVCM
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as L
import Kernel.Beam.Connection.Flow (prepareConnectionRider)
import Kernel.Beam.Connection.Types (ConnectionConfigRider (..))
import Kernel.Beam.Types (KafkaConn (..), Tables (..))
import Kernel.Prelude
import Kernel.Tools.LoopGracefully (loopGracefully)
import qualified Kernel.Tools.Metrics.Init as Metrics
import Kernel.Types.Flow (runFlowR)
import qualified Kernel.Utils.Common as KUC
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Time ()
import Network.HTTP.Types (status200, status503)
import Network.Wai (Application, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Producer.Flow as PF
import System.Environment (lookupEnv)
import qualified UnliftIO.Async as Async

getDhallName :: ProducerType -> String
getDhallName = \case
  Driver -> "producer"
  Rider -> "rider-producer"

-- | Default port for the producer health-check HTTP server. Overridable via
-- the @PRODUCER_HEALTHCHECK_PORT@ environment variable so no dhall/config
-- schema change is required (keeps existing deployments backward compatible).
defaultHealthCheckPort :: Int
defaultHealthCheckPort = 8080

startProducer :: IO ()
startProducer = do
  producerType <- fromMaybe Driver . (>>= readMaybe) <$> lookupEnv "PRODUCER_TYPE"
  appCfg :: AppCfg <- readDhallConfigDefault $ getDhallName (T.traceShowId producerType)
  Metrics.serve (appCfg.metricsPort)
  appEnv <- buildAppEnv appCfg producerType
  flowRt <- L.createFlowRuntime' (Just $ L.getEulerLoggerRuntime appEnv.hostname appEnv.loggerConfig)
  startProducerWithEnv flowRt appCfg appEnv producerType

startProducerWithEnv :: L.FlowRuntime -> AppCfg -> AppEnv -> ProducerType -> IO ()
startProducerWithEnv flowRt appCfg appEnv producerType = do
  runFlow
    flowRt
    ( ( prepareConnectionRider
          ( ConnectionConfigRider
              { esqDBCfg = appCfg.esqDBCfg,
                esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                hedisClusterCfg = appCfg.hedisClusterCfg,
                hedisSecondaryClusterCfg = appCfg.hedisSecondaryClusterCfg
              }
          )
          appCfg.kvConfigUpdateFrequency
      )
        >> L.setOption KafkaConn appEnv.kafkaProducerTools
        >> L.setOption Tables KUC.defaultTableData
        >> L.setOption KVCM.KVMetricCfg appEnv.coreMetrics.kvRedisMetricsContainer
    )
  let producers = map (\_ -> PF.runProducer) [1 .. appCfg.producersPerPod]
  let looperFlows = bool producers ([PF.runReviver producerType] <> producers) appEnv.runReviver
  healthCheckPort <- fromMaybe defaultHealthCheckPort . (>>= readMaybe) <$> lookupEnv "PRODUCER_HEALTHCHECK_PORT"
  -- Run the producer looper(s) in the background and expose a health-check
  -- HTTP server on the foreground thread. The server reports 200 only when the
  -- looper thread is alive AND Redis is reachable, otherwise 503.
  Async.withAsync (runFlowR flowRt appEnv $ loopGracefully looperFlows) $ \looperThread ->
    Warp.run healthCheckPort (healthCheckApp appEnv looperThread)

-- | WAI application backing the producer health-check endpoint. Responds on any
-- path so it works as a Kubernetes liveness/readiness probe target.
healthCheckApp :: AppEnv -> Async.Async () -> Application
healthCheckApp appEnv looperThread _req respond = do
  mLooperResult <- Async.poll looperThread
  redisOk <- checkRedisConnectivity appEnv
  case (mLooperResult, redisOk) of
    -- 'poll' returns Nothing while the looper async is still running.
    (Nothing, True) ->
      respond $ responseLBS status200 [("Content-Type", "text/plain")] "App is up"
    _ ->
      respond $ responseLBS status503 [("Content-Type", "text/plain")] "Producer unhealthy"

-- | Verify Redis connectivity with a PING -> PONG round-trip. Any exception or
-- non-PONG reply is treated as unhealthy.
checkRedisConnectivity :: AppEnv -> IO Bool
checkRedisConnectivity appEnv = do
  eRes <- try $ Redis.runRedis appEnv.hedisClusterEnv.hedisConnection Redis.ping
  pure $ case (eRes :: Either SomeException (Either Redis.Reply Redis.Status)) of
    Right (Right Redis.Pong) -> True
    _ -> False
