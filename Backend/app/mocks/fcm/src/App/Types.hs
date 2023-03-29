{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.Types where

import qualified Data.Map as Map
import EulerHS.Prelude
import Kernel.Types.App
import Kernel.Types.Common hiding (id)
import Kernel.Utils.App (getPodName)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Shutdown
import Lib.FCM.Types

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

data AppEnv = AppEnv
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    notificationsMap :: MVar (Map.Map FCMRecipientToken [FCMMessage Value]),
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- getPodName
  notificationsMap <- newMVar Map.empty
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
