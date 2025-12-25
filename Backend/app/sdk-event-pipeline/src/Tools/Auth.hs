{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth where

import Control.Arrow
import Data.Aeson
import Data.Aeson.Types
import Data.List (lookup)
import Data.Text as T
import Environment
import EulerHS.Prelude hiding (fromList, (.~))
import External.Flow (getDriverId)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.ARTUtils (HasARTFlow)
import Kernel.Tools.Metrics.CoreMetrics (HasCoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (HasLog)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl (..))
import Kernel.Utils.Servant.Server (HasEnvEntry (..), runFlowRDelayedIO)
import qualified Network.Wai as Wai
import Servant
  ( FromHttpApiData (parseHeader),
    HasServer (..),
    type (:>),
  )
import Servant.Client (HasClient (..))
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, withRequest)

data ClientType = RIDER | DRIVER | METRO_WEBVIEW

instance FromJSON ClientType where
  parseJSON (String "RiderApp") = return RIDER
  parseJSON (String "DriverApp") = return DRIVER
  parseJSON (String "MetroWebview") = return METRO_WEBVIEW
  parseJSON wrongVal = typeMismatch "Invalid ClientType" wrongVal

instance ToJSON ClientType where
  toJSON RIDER = String "RiderApp"
  toJSON DRIVER = String "DriverApp"
  toJSON METRO_WEBVIEW = String "MetroWebview"

data TokenAuth (tokenHeader :: Symbol) (clientTypeHeader :: Symbol)

type TokenAuthResult = (Text, ClientType)

-- | This server part implementation accepts a signature in @header@ and
instance
  ( HasServer api ctx,
    HasEnvEntry r ctx,
    KnownSymbol tokenHeader,
    KnownSymbol clientTypeHeader,
    Redis.HedisFlowEnv r,
    HasField "driverAppConfig" r DriverAppConfig,
    HasLog r,
    HasCoreMetrics r,
    HasCacheConfig r,
    HasARTFlow r
  ) =>
  HasServer (TokenAuth tokenHeader clientTypeHeader :> api) ctx
  where
  type
    ServerT (TokenAuth tokenHeader clientTypeHeader :> api) m =
      TokenAuthResult -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest authCheck
    where
      authCheck :: Wai.Request -> DelayedIO TokenAuthResult
      authCheck req = runFlowRDelayedIO env . withLogTag "authCheck" $ do
        token <-
          Wai.requestHeaders req
            & (lookup tokenHeaderName >>> fromMaybeM (MissingHeader tokenHeaderName))
            >>= (parseHeader >>> fromEitherM (InvalidHeader tokenHeaderName))
        clientType <-
          Wai.requestHeaders req
            & (lookup clientTypeHeaderName >>> fromMaybeM (MissingHeader clientTypeHeaderName))
            >>= (parseHeader >>> fromEitherM (InvalidHeader clientTypeHeaderName))
        case T.unpack clientType of
          "RiderApp" -> throwError AccessDenied
          "DriverApp" -> do
            result :: Maybe Text <- Redis.safeGet $ driverAppAuthKey token
            case result of
              Just personId -> return (personId, DRIVER)
              Nothing -> do
                eitherPersonId <- getDriverId token
                personId <-
                  case eitherPersonId of
                    Right personId -> return personId
                    Left _ -> throwError AccessDenied
                Redis.set (driverAppAuthKey token) personId
                return (personId, DRIVER)
          _ -> throwError AccessDenied
      driverAppAuthKey token = "driverAppAuth:" <> token
      tokenHeaderName = fromString $ symbolVal (Proxy @tokenHeader)
      tokenHeaderName :: IsString a => a
      clientTypeHeaderName = fromString $ symbolVal (Proxy @clientTypeHeader)
      clientTypeHeaderName :: IsString a => a
      env = getEnvEntry ctx
  hoistServerWithContext _ ctxp hst serv =
    hoistServerWithContext (Proxy @api) ctxp hst . serv

instance
  (HasClient m api, KnownSymbol tokenHeader, KnownSymbol clientTypeHeader) =>
  HasClient m (TokenAuth tokenHeader clientTypeHeader :> api)
  where
  type Client m (TokenAuth tokenHeader clientTypeHeader :> api) = Client m api

  clientWithRoute mp _ = clientWithRoute mp (Proxy @api)

  hoistClientMonad mp _ = hoistClientMonad mp (Proxy @api)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (TokenAuth tokenHeader clientTypeHeader :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)
