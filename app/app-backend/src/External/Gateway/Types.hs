{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module External.Gateway.Types where

import qualified Beckn.Types.API.Cancel as Cancel
import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Feedback as Feedback
import qualified Beckn.Types.API.Search as Search
import qualified Beckn.Types.API.Status as Status
import qualified Beckn.Types.API.Track as Track
import Beckn.Types.Common (AckResponse)
import Beckn.Utils.Servant.Trail.Client (RequestInfo, withClientTracing)
import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import qualified EulerHS.Types as ET
import Servant
import Types.API.Location

confirmAPI :: Proxy (Confirm.ConfirmAPI v)
confirmAPI = Proxy

confirm :: Text -> Confirm.ConfirmReq -> (RequestInfo, ET.EulerClient AckResponse)
confirm = ET.client $ withClientTracing confirmAPI

search :: Text -> Search.SearchReq -> (RequestInfo, ET.EulerClient AckResponse)
_ :<|> search = client $ withClientTracing Search.searchAPI

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

trackTripAPI :: Proxy (Track.TrackAPI v)
trackTripAPI = Proxy

trackTrip :: Text -> Track.TrackTripReq -> (RequestInfo, ET.EulerClient AckResponse)
trackTrip = client $ withClientTracing trackTripAPI

cancelAPI :: Proxy (Cancel.CancelAPI v)
cancelAPI = Proxy

cancel :: Text -> Cancel.CancelReq -> (RequestInfo, ET.EulerClient AckResponse)
cancel = client $ withClientTracing cancelAPI

statusAPI :: Proxy (Status.StatusAPI v)
statusAPI = Proxy

status :: Text -> Status.StatusReq -> (RequestInfo, ET.EulerClient AckResponse)
status = client $ withClientTracing statusAPI

feedbackAPI :: Proxy (Feedback.FeedbackAPI v)
feedbackAPI = Proxy

feedback :: Text -> Feedback.FeedbackReq -> (RequestInfo, ET.EulerClient AckResponse)
feedback = client $ withClientTracing feedbackAPI
