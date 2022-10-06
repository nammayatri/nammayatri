{-# LANGUAGE TypeApplications #-}

module Tools.Client where

import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Utils.Common hiding (Error, callAPI)
import Beckn.Utils.Dhall (FromDhall)
import qualified Domain.Types.RegistrationToken as DReg
import qualified EulerHS.Types as ET
import Servant hiding (throwError)
import Servant.Client
import "app-backend" Tools.Auth (DashboardTokenAuth)
import Tools.Error
import Tools.Metrics

callAppBackendYatriApi,
  callAppBackendArduApi,
  callBecknTransportApi,
  callDriverOfferApi ::
    ( HasCallStack,
      CoreMetrics m,
      HasFlowEnv m r '["dataServers" ::: [DataServer]],
      ET.JSONEx res,
      ToJSON res
    ) =>
    (RegToken -> ET.EulerClient res) ->
    Text ->
    m res
callAppBackendYatriApi = callHelperAPI DReg.APP_BACKEND_YATRI
callAppBackendArduApi = callHelperAPI DReg.APP_BACKEND_ARDU
callBecknTransportApi = callHelperAPI DReg.BECKN_TRANSPORT
callDriverOfferApi = callHelperAPI DReg.DRIVER_OFFER_BPP

callHelperAPI ::
  ( HasCallStack,
    CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    ET.JSONEx res,
    ToJSON res
  ) =>
  DReg.ServerName ->
  (RegToken -> ET.EulerClient res) ->
  Text ->
  m res
callHelperAPI serverName cl desc = do
  dataServer <- getDataServer serverName
  callAPI dataServer.url (cl dataServer.token) desc

callAPI :: CallAPI env res
callAPI = callApiUnwrappingApiError (identity @Error) Nothing Nothing

client ::
  forall api.
  HasClient ET.EulerClient api =>
  Proxy api ->
  Client ET.EulerClient (DashboardTokenAuth :> api)
client _ = ET.client (Proxy @(DashboardTokenAuth :> api))

data DataServer = DataServer
  { name :: DReg.ServerName,
    url :: BaseUrl,
    token :: Text
  }
  deriving (Generic, FromDhall)

getDataServer ::
  HasFlowEnv m r '["dataServers" ::: [DataServer]] =>
  DReg.ServerName ->
  m DataServer
getDataServer serverName = do
  dataServers <- asks (.dataServers)
  case filter (\server -> server.name == serverName) dataServers of
    [dataServer] -> pure dataServer
    [] -> throwError (InternalError $ "Unknown data server: " <> show serverName)
    _ -> throwError (InternalError $ "Should be exactly one data server with name " <> show serverName)
