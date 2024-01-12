module Main where

-- import Config.Config as Config
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
import qualified EulerHS.Language as L
import EulerHS.Logger.Types
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as ET
import qualified Event.Event as Event
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types (ConnectionConfigDriver (..))
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Utils.Dhall hiding (void)
import qualified Kernel.Utils.FlowLogging as L
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import Types.DBSync as TDB
import Utils.Utils

main :: IO ()
main = do
  appCfg <- (id :: AppCfg -> AppCfg) <$> readDhallConfigDefault "dynamic-offer-driver-app"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  kafkaProducerTools <- buildKafkaProducerTools' appCfg.kafkaProducerCfg appCfg.maxMessages
  bracket (async NW.runMetricServer) cancel $ \_ -> do
    R.withFlowRuntime
      (Just loggerRt)
      ( \flowRt -> do
          putStrLn @String "Initializing DB and KV Connections..."
          runFlow
            flowRt
            ( prepareConnectionDriver
                ConnectionConfigDriver
                  { esqDBCfg = appCfg.esqDBCfg,
                    esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                    hedisClusterCfg = appCfg.hedisClusterCfg
                  }
                appCfg.kvConfigUpdateFrequency
            )
          dbSyncMetric <- Event.mkDBSyncMetric
          threadPerPodCount <- Env.getThreadPerPodCount
          let environment = Env (T.pack C.kvRedis) dbSyncMetric kafkaProducerTools.producer appCfg.dontEnableForDb
              totalShards = C.numberOfStreamsForKV
              numberOfShardPerThread = totalShards `div` (threadPerPodCount + 1)
          lastShards <- spawnDrainerThread threadPerPodCount flowRt environment numberOfShardPerThread (0, numberOfShardPerThread)
          R.runFlow flowRt (runReaderT (DBSync.startDBSync (lastShards, totalShards) (threadPerPodCount + 1)) environment)
      )

spawnDrainerThread :: Integer -> R.FlowRuntime -> TDB.Env -> Integer -> (Integer, Integer) -> IO Integer
spawnDrainerThread 0 _ _ _ (startShardValue, _) = pure startShardValue
spawnDrainerThread count flowRt env numberOfShardPerThread (startShardValue, lastShardValue) = do
  threadId <- forkIO $ R.runFlow flowRt (runReaderT (DBSync.startDBSync (startShardValue, lastShardValue) count) env)
  spawnDrainerThread (count -1) flowRt env numberOfShardPerThread (lastShardValue, lastShardValue + numberOfShardPerThread)
