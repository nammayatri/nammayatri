{-# LANGUAGE TypeApplications #-}

module Tools.Client where

import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Utils.Common hiding (Error, callAPI)
import Beckn.Utils.Dhall (FromDhall)
import qualified EulerHS.Types as ET
import Tools.Error
import Tools.Metrics

callAppBackendApi,
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
callAppBackendApi = callHelperAPI APP_BACKEND
callBecknTransportApi = callHelperAPI BECKN_TRANSPORT
callDriverOfferApi = callHelperAPI DRIVER_OFFER_BPP

callHelperAPI ::
  ( HasCallStack,
    CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    ET.JSONEx res,
    ToJSON res
  ) =>
  ServerName ->
  (RegToken -> ET.EulerClient res) ->
  Text ->
  m res
callHelperAPI serverName client desc = do
  dataServer <- getDataServer serverName
  callAPI dataServer.url (client dataServer.token) desc

callAPI :: CallAPI env res
callAPI = callApiUnwrappingApiError (identity @Error) Nothing Nothing

data ServerName = APP_BACKEND | BECKN_TRANSPORT | DRIVER_OFFER_BPP
  deriving (Generic, FromDhall, Eq, Show)

data DataServer = DataServer
  { name :: ServerName,
    url :: BaseUrl,
    token :: Text
  }
  deriving (Generic, FromDhall)

getDataServer ::
  HasFlowEnv m r '["dataServers" ::: [DataServer]] =>
  ServerName ->
  m DataServer
getDataServer serverName = do
  dataServers <- asks (.dataServers)
  case filter (\server -> server.name == serverName) dataServers of
    [dataServer] -> pure dataServer
    [] -> throwError (InternalError $ "Unknown data server: " <> show serverName)
    _ -> throwError (InternalError $ "Should be exactly one data server with name " <> show serverName)
