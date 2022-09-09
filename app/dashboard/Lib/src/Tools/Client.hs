{-# LANGUAGE TypeApplications #-}

module Tools.Client where

import Beckn.Prelude
import Beckn.Utils.Common hiding (Error, callAPI)
import qualified EulerHS.Types as ET
import Tools.DataServer
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
