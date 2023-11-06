{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Tools.Client (CallServerAPI (..), clientWithMerchantAndCity) where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.ServerName as DSN
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Utils.Common hiding (Error, callAPI)
import Servant hiding (throwError)
import Servant.Client hiding (client)
import Tools.Auth.Merchant
import Tools.Error
import Tools.Metrics

getDataServer ::
  HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]] =>
  DSN.ServerName ->
  m DSN.DataServer
getDataServer serverName = do
  dataServers <- asks (.dataServers)
  case filter (\server -> server.name == serverName) dataServers of
    [dataServer] -> pure dataServer
    [] -> throwError (InternalError $ "Unknown data server: " <> show serverName)
    _ -> throwError (InternalError $ "Should be exactly one data server with name " <> show serverName)

class
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]]
  ) =>
  CallServerAPI apis m r b c
    | m b -> c
  where
  callServerAPI :: DSN.ServerName -> (Text -> apis) -> Text -> (apis -> b) -> c

instance
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]],
    ToJSON d,
    FromJSON d
  ) =>
  CallServerAPI apis m r (Euler.EulerClient d) (m d)
  where
  callServerAPI serverName mkAPIs descr f = do
    dataServer <- getDataServer serverName
    let driverOfferAPIs = mkAPIs dataServer.token
    callApiUnwrappingApiError (identity @Error) Nothing Nothing dataServer.url (f driverOfferAPIs) descr (Proxy :: Proxy Raw)

instance
  ( CoreMetrics m,
    HasFlowEnv m k '["dataServers" ::: [DSN.DataServer]],
    MonadFlow m,
    CallServerAPI apis m k d r1,
    r ~ (c -> r1)
  ) =>
  CallServerAPI apis m k (c -> d) r
  where
  callServerAPI serverName mkAPIs descr f c =
    callServerAPI @_ @m serverName mkAPIs descr (`f` c)

type ApiWithMerchantAndCity api =
  "dashboard"
    :> Capture "merchantId" (CheckedShortId DM.Merchant)
    :> Capture "city" City.City
    :> api

clientWithMerchantAndCity ::
  forall api.
  HasClient Euler.EulerClient api =>
  Proxy api ->
  Client Euler.EulerClient (ApiWithMerchantAndCity api)
clientWithMerchantAndCity _ = Euler.client (Proxy @(ApiWithMerchantAndCity api))
