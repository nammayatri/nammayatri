{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module External.Gateway.Types where

import qualified Beckn.Types.Core.API.Cancel as Cancel
import qualified Beckn.Types.Core.API.Confirm as Confirm
import qualified Beckn.Types.Core.API.Feedback as Feedback
import qualified Beckn.Types.Core.API.Search as Search
import qualified Beckn.Types.Core.API.Status as Status
import qualified Beckn.Types.Core.API.Track as Track
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Utils.Servant.Trail.Client (RequestInfo, withClientTracing)
import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import qualified EulerHS.Types as ET
import Servant
import Types.API.Location

confirmAPI :: Proxy Confirm.ConfirmAPI
confirmAPI = Proxy

confirm :: Confirm.ConfirmReq -> (RequestInfo, ET.EulerClient AckResponse)
confirm = ET.client $ withClientTracing confirmAPI

searchAPI :: Proxy Search.SearchAPI
searchAPI = Proxy

search :: Search.SearchReq -> (RequestInfo, EulerClient AckResponse)
search = client $ withClientTracing searchAPI

nsdlSearch :: Search.SearchReq -> (RequestInfo, ET.EulerClient AckResponse)
nsdlSearch = client $ withClientTracing Search.nsdlSearchAPI

type LocationAPI =
  "location"
    :> Capture "caseId" Text
    :> Get '[JSON] GetLocationRes

locationAPI :: Proxy LocationAPI
locationAPI = Proxy

location :: Text -> (RequestInfo, EulerClient GetLocationRes)
location = client $ withClientTracing locationAPI

trackTripAPI :: Proxy Track.TrackAPI
trackTripAPI = Proxy

trackTrip :: Track.TrackTripReq -> (RequestInfo, ET.EulerClient AckResponse)
trackTrip = client $ withClientTracing trackTripAPI

cancelAPI :: Proxy Cancel.CancelAPI
cancelAPI = Proxy

cancel :: Cancel.CancelReq -> (RequestInfo, ET.EulerClient AckResponse)
cancel = client $ withClientTracing cancelAPI

statusAPI :: Proxy Status.StatusAPI
statusAPI = Proxy

status :: Status.StatusReq -> (RequestInfo, ET.EulerClient AckResponse)
status = client $ withClientTracing statusAPI

feedbackAPI :: Proxy Feedback.FeedbackAPI
feedbackAPI = Proxy

feedback :: Feedback.FeedbackReq -> (RequestInfo, ET.EulerClient AckResponse)
feedback = client $ withClientTracing feedbackAPI
