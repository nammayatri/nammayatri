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
import qualified Producer.Flow as PF
import System.Environment (lookupEnv)

-- import qualified EulerHS.Language as L

getDhallName :: ProducerType -> String
getDhallName = \case
  Driver -> "producer"
  Rider -> "rider-producer"

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
                hedisClusterCfg = appCfg.hedisClusterCfg
              }
          )
          appCfg.kvConfigUpdateFrequency
      )
        >> L.setOption KafkaConn appEnv.kafkaProducerTools
        >> L.setOption Tables KUC.defaultTableData
        >> L.setOption KVCM.KVMetricCfg appEnv.coreMetrics.kvRedisMetricsContainer
    )

  let producersWithCounter = concatMap (\_ -> map (\streamIndex -> PF.runProducer streamIndex) [1 .. 16]) [1 .. appCfg.producersPerPod]
  let reviverWithCounter = map (\streamIndex -> PF.runReviver producerType streamIndex) [1 .. 16]
  putStrLn $ ("StreamName is now: " :: String)
  runFlowR flowRt appEnv $ do
    loopGracefully $
      bool producersWithCounter (reviverWithCounter ++ producersWithCounter) appEnv.runReviver
