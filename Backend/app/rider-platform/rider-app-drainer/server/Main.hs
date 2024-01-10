module Main where

import Config.Env as Env
import qualified Constants as C
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, cancel)
import qualified DBSync.DBSync as DBSync
import qualified Data.HashSet as HS
import qualified "unordered-containers" Data.HashSet as HashSet
import qualified Data.Text as T
import Environment
import qualified Euler.Events.Network as NW
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Interpreters as R
import EulerHS.Logger.Types
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as ET
import qualified Event.Event as Event
import Kernel.Beam.Connection.Flow (prepareConnectionRider)
import Kernel.Beam.Connection.Types (ConnectionConfigRider (..))
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Utils.Dhall hiding (void)
import qualified Kernel.Utils.FlowLogging as L
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import Types.DBSync as TDB
import Utils.Utils

main :: IO ()
main = do
  appCfg <- (id :: AppCfg -> AppCfg) <$> readDhallConfigDefault "rider-app"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  kafkaProducerTools <- buildKafkaProducerTools' appCfg.kafkaProducerCfg appCfg.kafkaProperties
  bracket (async NW.runMetricServer) cancel $ \_ -> do
    R.withFlowRuntime
      (Just loggerRt)
      ( \flowRt -> do
          putStrLn @String "Initializing DB and KV Connections..."
          runFlow
            flowRt
            ( prepareConnectionRider
                ConnectionConfigRider
                  { esqDBCfg = appCfg.esqDBCfg,
                    esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                    hedisClusterCfg = appCfg.hedisClusterCfg
                  }
                appCfg.kvConfigUpdateFrequency
            )

          dbSyncMetric <- Event.mkDBSyncMetric
          let environment = Env (T.pack C.kvRedis) dbSyncMetric kafkaProducerTools.producer appCfg.dontEnableForDb
          threadPerPodCount <- Env.getThreadPerPodCount
          spawnDrainerThread threadPerPodCount flowRt environment
          R.runFlow flowRt (runReaderT DBSync.startDBSync environment)
      )

spawnDrainerThread :: Int -> R.FlowRuntime -> TDB.Env -> IO ()
spawnDrainerThread 0 _ _ = pure ()
spawnDrainerThread count flowRt env = do
  void . forkIO $ R.runFlow flowRt (runReaderT DBSync.startDBSync env)
  spawnDrainerThread (count -1) flowRt env
