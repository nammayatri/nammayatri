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
import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import qualified EulerHS.Types as ET
import Servant
import Types.API.Location

confirmAPI :: Proxy Confirm.ConfirmAPI
confirmAPI = Proxy

confirm :: Confirm.ConfirmReq -> ET.EulerClient AckResponse
confirm = ET.client confirmAPI

searchAPI :: Proxy Search.SearchAPI
searchAPI = Proxy

search :: Search.SearchReq -> EulerClient AckResponse
search = client searchAPI

nsdlSearch :: Search.SearchReq -> ET.EulerClient AckResponse
nsdlSearch = client Search.nsdlSearchAPI

type LocationAPI =
  "location"
    :> Capture "caseId" Text
    :> Get '[JSON] GetLocationRes

locationAPI :: Proxy LocationAPI
locationAPI = Proxy

location :: Text -> EulerClient GetLocationRes
location = client locationAPI

trackTripAPI :: Proxy Track.TrackAPI
trackTripAPI = Proxy

trackTrip :: Track.TrackTripReq -> ET.EulerClient AckResponse
trackTrip = client trackTripAPI

cancelAPI :: Proxy Cancel.CancelAPI
cancelAPI = Proxy

cancel :: Cancel.CancelReq -> ET.EulerClient AckResponse
cancel = client cancelAPI

statusAPI :: Proxy Status.StatusAPI
statusAPI = Proxy

status :: Status.StatusReq -> ET.EulerClient AckResponse
status = client statusAPI

feedbackAPI :: Proxy Feedback.FeedbackAPI
feedbackAPI = Proxy

feedback :: Feedback.FeedbackReq -> ET.EulerClient AckResponse
feedback = client feedbackAPI
