module External.Gateway.API where

import Beckn.Types.Core.API.Call
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Search (OnSearchReq, OnSearchRes, nsdlOnSearchAPI)
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.API.Update
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Utils.Servant.HeaderAuth (HeaderAuthKey)
import Beckn.Utils.Servant.Trail.Client (RequestInfo, withClientTracing)
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant

type OnSearchAPI =
  "on_search"
    :> HeaderAuthKey
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] OnSearchRes

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy

onSearch :: Text -> OnSearchReq -> (RequestInfo, ET.EulerClient AckResponse)
onSearch callbackApiKey = ET.client (withClientTracing onSearchAPI) (Just callbackApiKey)

nsdlOnSearch :: OnSearchReq -> (RequestInfo, ET.EulerClient AckResponse)
nsdlOnSearch = ET.client $ withClientTracing nsdlOnSearchAPI

type OnTrackAPI =
  "on_track"
    :> HeaderAuthKey
    :> ReqBody '[JSON] OnTrackTripReq
    :> Post '[JSON] OnTrackTripRes

onTrackTripAPI :: Proxy OnTrackAPI
onTrackTripAPI = Proxy

onTrackTrip :: Text -> OnTrackTripReq -> (RequestInfo, ET.EulerClient AckResponse)
onTrackTrip callbackApiKey = ET.client (withClientTracing onTrackTripAPI) (Just callbackApiKey)

type OnConfirmAPI =
  "on_confirm"
    :> HeaderAuthKey
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy

onConfirm :: Text -> OnConfirmReq -> (RequestInfo, ET.EulerClient AckResponse)
onConfirm callbackApiKey = ET.client (withClientTracing onConfirmAPI) (Just callbackApiKey)

type OnCancelAPI =
  "on_cancel"
    :> HeaderAuthKey
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

cancelAPI :: Proxy OnCancelAPI
cancelAPI = Proxy

onCancel :: Text -> OnCancelReq -> (RequestInfo, ET.EulerClient AckResponse)
onCancel callbackApiKey = ET.client (withClientTracing cancelAPI) (Just callbackApiKey)

type OnStatusAPI =
  "on_status"
    :> HeaderAuthKey
    :> ReqBody '[JSON] OnStatusReq
    :> Post '[JSON] OnStatusRes

statusAPI :: Proxy OnStatusAPI
statusAPI = Proxy

onStatus :: Text -> OnStatusReq -> (RequestInfo, ET.EulerClient AckResponse)
onStatus callbackApiKey = ET.client (withClientTracing statusAPI) (Just callbackApiKey)

type CallAPI =
  "call"
    :> "to_customer"
    :> ReqBody '[JSON] CallReq
    :> Post '[JSON] CallRes

callsAPI :: Proxy CallAPI
callsAPI = Proxy

initiateCall :: CallReq -> (RequestInfo, ET.EulerClient AckResponse)
initiateCall = ET.client $ withClientTracing callsAPI

type UpdateAPI =
  "on_update"
    :> HeaderAuthKey
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] OnUpdateRes

updateAPI :: Proxy UpdateAPI
updateAPI = Proxy

onUpdate :: Text -> OnUpdateReq -> (RequestInfo, ET.EulerClient AckResponse)
onUpdate callbackApiKey = ET.client (withClientTracing updateAPI) (Just callbackApiKey)
