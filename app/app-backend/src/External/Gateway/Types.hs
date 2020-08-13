{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module External.Gateway.Types where

import qualified Beckn.Types.API.Cancel as Cancel
import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import qualified Beckn.Types.API.Status as Status
import Beckn.Types.API.Track
import Beckn.Types.Common (AckResponse)
import Beckn.Utils.Servant.Trail.Client (RequestInfo, withClientTracing)
import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import qualified EulerHS.Types as ET
import Servant
import Types.API.Location

type ConfirmAPI =
  "confirm" :> ReqBody '[JSON] Confirm.ConfirmReq :> Post '[JSON] Confirm.ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

confirm :: Confirm.ConfirmReq -> (RequestInfo, ET.EulerClient AckResponse)
confirm = ET.client $ withClientTracing confirmAPI

search :: Text -> Search.SearchReq -> (RequestInfo, ET.EulerClient AckResponse)
search = client $ withClientTracing Search.searchAPI

nsdlSearch :: Maybe Text -> Maybe Text -> Search.SearchReq -> (RequestInfo, ET.EulerClient AckResponse)
nsdlSearch = client $ withClientTracing Search.nsdlSearchAPI

type LocationAPI =
  "location"
    :> Capture "caseId" Text
    :> Get '[JSON] GetLocationRes

locationAPI :: Proxy LocationAPI
locationAPI = Proxy

location :: Text -> (RequestInfo, EulerClient GetLocationRes)
location = client $ withClientTracing locationAPI

type TrackTripAPI =
  "track"
    :> ReqBody '[JSON] TrackTripReq
    :> Post '[JSON] TrackTripRes

trackTripAPI :: Proxy TrackTripAPI
trackTripAPI = Proxy

trackTrip :: TrackTripReq -> (RequestInfo, ET.EulerClient AckResponse)
trackTrip = client $ withClientTracing trackTripAPI

type CancelAPI =
  "cancel"
    :> ReqBody '[JSON] Cancel.CancelReq
    :> Post '[JSON] Cancel.CancelRes

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

cancel :: Cancel.CancelReq -> (RequestInfo, ET.EulerClient AckResponse)
cancel = client $ withClientTracing cancelAPI

type StatusAPI =
  "status"
    :> ReqBody '[JSON] Status.StatusReq
    :> Post '[JSON] Status.StatusRes

statusAPI :: Proxy StatusAPI
statusAPI = Proxy

status :: Status.StatusReq -> (RequestInfo, ET.EulerClient AckResponse)
status = client $ withClientTracing statusAPI
