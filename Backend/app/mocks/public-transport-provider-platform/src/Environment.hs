{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import Control.Monad.Catch (bracket)
import Kernel.Mock.ExternalAPI
import Kernel.Storage.Hedis
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.SignatureAuth hiding (prepareAuthManager)
import Network.HTTP.Client (Manager, newManager)
import Relude

data AppCfg = AppCfg
  { port :: Int,
    selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    hedisCfg :: HedisCfg,
    statusWaitTimeSec :: Seconds,
    callbackWaitTimeMilliSec :: Milliseconds,
    loggerConfig :: LoggerConfig,
    authEntity :: AuthenticatingEntity'
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    statusWaitTimeSec :: Seconds,
    callbackWaitTimeMilliSec :: Milliseconds,
    loggerConfig :: LoggerConfig,
    authEntity :: AuthenticatingEntity',
    hedisEnv :: HedisEnv,
    loggerEnv :: LoggerEnv,
    authManager :: Manager
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hedisEnv <- connectHedis hedisCfg ("mock_public_transport_provider_platform" <>)
  loggerEnv <- prepareLoggerEnv loggerConfig Nothing
  let authManagerSettings = prepareAuthManager config ["Authorization"] selfId uniqueKeyId (logOutputIO loggerEnv)
  authManager <- newManager authManagerSettings
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  disconnectHedis hedisEnv
  releaseLoggerEnv loggerEnv

withAppEnv :: AppCfg -> (AppEnv -> IO ()) -> IO ()
withAppEnv cfg = bracket (buildAppEnv cfg) releaseAppEnv

instance AuthenticatingEntity AppCfg where
  getSigningKey = (.authEntity.signingKey)
  getSignatureExpiry = (.authEntity.signatureExpiry)
