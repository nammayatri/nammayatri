{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App (startProducer) where

import Data.Function
import Environment
import qualified EulerHS.Runtime as L
import Kernel.Prelude
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.Common ()
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import qualified Producer.Flow as PF

startProducer :: IO ()
startProducer = do
  appCfg :: AppCfg <- readDhallConfigDefault "producer"
  appEnv <- buildAppEnv appCfg
  flowRt <- L.createFlowRuntime' (Just $ L.getEulerLoggerRuntime appEnv.hostname appEnv.loggerConfig)
  startProducerWithEnv flowRt appEnv

startProducerWithEnv :: L.FlowRuntime -> AppEnv -> IO ()
startProducerWithEnv flowRt appEnv@AppEnv {} = do
  runFlowR flowRt appEnv PF.runProducer
