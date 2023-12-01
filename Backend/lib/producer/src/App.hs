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
import Environment
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as L
import Kernel.Beam.Connection.Flow (prepareConnectionRider)
import Kernel.Beam.Connection.Types (ConnectionConfigRider (..))
import Kernel.Beam.Types (KafkaConn (..))
import Kernel.Prelude
import Kernel.Tools.LoopGracefully (loopGracefully)
import qualified Kernel.Tools.Metrics.Init as Metrics
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Time ()
import qualified Producer.Flow as PF

startProducer :: IO ()
startProducer = do
  appCfg :: AppCfg <- readDhallConfigDefault "producer"
  Metrics.serve (appCfg.metricsPort)
  appEnv <- buildAppEnv appCfg
  flowRt <- L.createFlowRuntime' (Just $ L.getEulerLoggerRuntime appEnv.hostname appEnv.loggerConfig)
  startProducerWithEnv flowRt appCfg appEnv

startProducerWithEnv :: L.FlowRuntime -> AppCfg -> AppEnv -> IO ()
startProducerWithEnv flowRt appCfg appEnv = do
  runFlow
    flowRt
    ( ( prepareConnectionRider
          ( ConnectionConfigRider
              { esqDBCfg = appCfg.esqDBCfg,
                esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                hedisClusterCfg = appCfg.hedisClusterCfg
              }
          )
          appCfg.tables
      )
        >> L.setOption KafkaConn appEnv.kafkaProducerTools
    )
  runFlowR flowRt appEnv $ do
    loopGracefully $ bool [PF.runProducer] [PF.runReviver, PF.runProducer] appEnv.runReviver
