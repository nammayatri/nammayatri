{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module External.Gateway.Types where

import qualified Beckn.Types.API.Cancel as Cancel
import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.API.Track
import EulerHS.Language (Flow)
import EulerHS.Prelude
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import EulerHS.Types (client)
import Servant
import Types.API.Location

type ConfirmAPI =
  "confirm" :> "services" :> ReqBody '[JSON] Confirm.ConfirmReq :> Post '[JSON] Confirm.ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

confirm req =
  void $ ET.client confirmAPI req

type SearchAPI =
  "search"
    :> "services"
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search.SearchRes

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

search req =
  void $ client searchAPI req

type LocationAPI =
  "location"
    :> Capture "caseId" Text
    :> Get '[JSON] GetLocationRes

locationAPI :: Proxy LocationAPI
locationAPI = Proxy

location caseId =
  client locationAPI caseId

type TrackTripAPI =
  "track"
    :> "trip"
    :> ReqBody '[JSON] TrackTripReq
    :> Post '[JSON] TrackTripRes

trackTripAPI :: Proxy TrackTripAPI
trackTripAPI = Proxy

trackTrip req =
  void $ client trackTripAPI req

type CancelAPI =
  "cancel"
    :> "services"
    :> ReqBody '[JSON] Cancel.CancelReq
    :> Post '[JSON] Cancel.CancelRes

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

cancel req =
  void $ client cancelAPI req
