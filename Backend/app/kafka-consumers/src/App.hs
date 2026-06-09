{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App (startKafkaConsumer, runDriverHealthcheck) where

import Control.Concurrent hiding (threadDelay)
import Data.Function
import "dynamic-offer-driver-app" Domain.Types.Message (MessageDict)
import DriverTrackingHealthCheck.API
import qualified DriverTrackingHealthCheck.Service.Runner as Service
import Environment
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as L
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionRider)
import Kernel.Beam.Connection.Types (ConnectionConfigRider (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.Init as Metrics
import qualified Kernel.Types.App as App
import Kernel.Types.Error
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.Common hiding (id)
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import qualified Kernel.Utils.Servant.Server as Server
import Kernel.Utils.Shutdown
import Network.Wai.Handler.Warp
import qualified Processor.BroadcastMessage.Processor as BMProcessor
import qualified Processor.FleetCommunication.Processor as FCProcessor
import qualified Processor.LocationUpdate.Processor as LCProcessor
import qualified Processor.LocationUpdate.Types as LU
import qualified Processor.RideEvents.Processor as RideEventsProcessor
import Servant
import System.Environment (lookupEnv)
import SystemConfigsOverride as QSC hiding (id)
import qualified Transporter.Kafka.Flow as KafkaFlow
import qualified Transporter.RedisStream.Flow as RSFlow

startKafkaConsumer :: IO ()
startKafkaConsumer = do
  consumerType :: ConsumerType <- read . fromMaybe "RIDE_EVENTS_CONSUMER" <$> lookupEnv "CONSUMER_TYPE"
  configFile <- KafkaFlow.getConfigNameFromConsumertype consumerType
  appCfg :: AppCfg <- readDhallConfigDefault configFile
  appEnv <- buildAppEnv appCfg consumerType
  Metrics.serve appCfg.metricsPort
  -- LOCATION_UPDATE is the only consumer that also runs the driver health-check
  -- HTTP server in the same process. Both transport paths get it.
  when (consumerType == LOCATION_UPDATE) (void $ forkIO $ runDriverHealthcheck appCfg appEnv)
  startConsumer appCfg appEnv

-- | Bootstrap the EulerHS flow runtime + DB/Redis connections, then hand off
-- to whichever transport is configured.
startConsumer :: AppCfg -> AppEnv -> IO ()
startConsumer appCfg appEnv = do
  let loggerRuntime = L.getEulerLoggerRuntime appEnv.hostname appEnv.loggerConfig
  R.withFlowRuntime (Just loggerRuntime) $ \flowRt' -> do
    managers <- managersFromManagersSettings appCfg.httpClientOptions.timeoutMs mempty
    let flowRt = flowRt' {L._httpClientManagers = managers}
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
          >> L.setOption KBT.KafkaConn appEnv.kafkaProducerTools
      )
    flowRt'' <- runFlowR flowRt appEnv $ do
      fork
        "Fetching Kv configs"
        ( forever $ do
            kvConfigs <-
              QSC.findById "kv_configs" >>= pure . decodeFromText' @Tables
                >>= fromMaybeM (InternalError "Couldn't find kv_configs table for kafka consumer")
            L.setOption KBT.Tables kvConfigs
            threadDelay (appCfg.kvConfigUpdateFrequency * 1000000)
        )
      pure flowRt
    case appEnv.transport of
      Kafka -> startKafkaTransport flowRt'' appEnv
      RedisStream -> startRedisStreamTransport flowRt'' appEnv

------------------------------------------------------------
-- Kafka transport dispatch
------------------------------------------------------------

startKafkaTransport :: L.FlowRuntime -> AppEnv -> IO ()
startKafkaTransport flowRt appEnv = do
  kc <- KafkaFlow.newKafkaConsumer appEnv
  case appEnv.consumerType of
    RIDE_EVENTS_CONSUMER ->
      KafkaFlow.runPerEvent flowRt appEnv kc $ \event _key ->
        RideEventsProcessor.processRideEnded event
    BROADCAST_MESSAGE ->
      KafkaFlow.runPerEvent flowRt appEnv kc BMProcessor.broadcastMessage
    FLEET_COMMUNICATION_DISPATCH ->
      KafkaFlow.runPerEvent flowRt appEnv kc $ \payload _key ->
        FCProcessor.processFleetCommunicationDelivery payload
    LOCATION_UPDATE -> do
      let enabledCityIds = maybe [] (.enabledMerchantCityIds) appEnv.healthCheckAppCfg
          batchSize = maybe 100 (fromIntegral . (.batchSize)) appEnv.healthCheckAppCfg
      KafkaFlow.runBatch flowRt appEnv kc batchSize $ \batch ->
        LCProcessor.processLocationData enabledCityIds batch

------------------------------------------------------------
-- Redis-Stream transport dispatch
------------------------------------------------------------

startRedisStreamTransport :: L.FlowRuntime -> AppEnv -> IO ()
startRedisStreamTransport flowRt appEnv = do
  cfg <-
    appEnv.redisStreamCfg
      & maybe (error "RedisStream transport selected but redisStreamCfg is missing from dhall config") pure
  let instanceName = fromMaybe "kafka-consumers" appEnv.hostname
  case appEnv.consumerType of
    RIDE_EVENTS_CONSUMER ->
      RSFlow.run flowRt appEnv cfg instanceName RideEventsProcessor.processRideEnded
    BROADCAST_MESSAGE ->
      RSFlow.run flowRt appEnv cfg instanceName $ \BroadcastEntry {messageDict, driverId} ->
        BMProcessor.broadcastMessage messageDict driverId
    FLEET_COMMUNICATION_DISPATCH ->
      RSFlow.run flowRt appEnv cfg instanceName FCProcessor.processFleetCommunicationDelivery
    LOCATION_UPDATE -> do
      let enabledCityIds = maybe [] (.enabledMerchantCityIds) appEnv.healthCheckAppCfg
      RSFlow.runBatch flowRt appEnv cfg instanceName $ \(entries :: [LU.LocationEntry]) ->
        LCProcessor.processLocationData enabledCityIds (map (\e -> (e.locationUpdate, e.driverId)) entries)

-- | Wire format for BROADCAST_MESSAGE on the Redis-Stream transport.
-- Kafka carries the driver id in the message key; for RedisStream we bundle
-- it with the payload.
data BroadcastEntry = BroadcastEntry
  { messageDict :: MessageDict,
    driverId :: Text
  }
  deriving (Generic, FromJSON)

------------------------------------------------------------
-- Health-check sidecar (LOCATION_UPDATE only)
------------------------------------------------------------

runDriverHealthcheck :: AppCfg -> AppEnv -> IO ()
runDriverHealthcheck appCfg appEnv = do
  -- Metrics are now served for all consumer types in 'startKafkaConsumer'.
  let heathCheckConfig = fromJust appCfg.healthCheckAppCfg
  let loggerRt = L.getEulerLoggerRuntime appEnv.hostname heathCheckConfig.loggerConfig

  R.withFlowRuntime (Just loggerRt) \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv do
      managers <- createManagers mempty -- default manager is created
      pure $ flowRt {R._httpClientManagers = managers}

    let settings =
          defaultSettings
            & setGracefulShutdownTimeout (Just $ getSeconds heathCheckConfig.graceTerminationPeriod)
            & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
            & setPort heathCheckConfig.healthcheckPort
    void . forkIO . runSettings settings $ Server.run healthCheckAPI healthCheck EmptyContext (App.EnvR flowRt' appEnv)
    runFlow
      flowRt'
      ( prepareConnectionRider
          ( ConnectionConfigRider
              { esqDBCfg = appCfg.esqDBCfg,
                esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                hedisClusterCfg = appCfg.hedisClusterCfg,
                hedisSecondaryClusterCfg = appCfg.hedisSecondaryClusterCfg
              }
          )
          appCfg.kvConfigUpdateFrequency
      )
    flowRt'' <-
      runFlowR flowRt appEnv $ do
        fork
          "Fetching Kv configs"
          ( forever $ do
              kvConfigs <-
                QSC.findById "kv_configs" >>= pure . decodeFromText' @Tables
                  >>= fromMaybeM (InternalError "Couldn't find kv_configs table for kafka consumer")
              L.setOption KBT.Tables kvConfigs
              threadDelay (appCfg.kvConfigUpdateFrequency * 1000000)
          )
        pure flowRt'
    runFlowR flowRt'' appEnv $ Service.driverTrackingHealthcheckService heathCheckConfig
    waitForShutdown appEnv.isShuttingDown
