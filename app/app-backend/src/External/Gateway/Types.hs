{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module External.Gateway.Types where

import qualified Beckn.Types.API.Cancel as Cancel
import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import qualified Beckn.Types.API.Status as Status
import Beckn.Types.API.Track
import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import qualified EulerHS.Types as ET
import Servant
import Types.API.Location

type ConfirmAPI =
  "confirm" :> ReqBody '[JSON] Confirm.ConfirmReq :> Post '[JSON] Confirm.ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

confirm :: Confirm.ConfirmReq -> EulerClient ()
confirm req =
  void $ ET.client confirmAPI req

search :: Text -> Search.SearchReq -> EulerClient ()
search key req =
  void $ client Search.searchAPI key req

type LocationAPI =
  "location"
    :> Capture "caseId" Text
    :> Get '[JSON] GetLocationRes

locationAPI :: Proxy LocationAPI
locationAPI = Proxy

location :: Text -> EulerClient GetLocationRes
location = client locationAPI

type TrackTripAPI =
  "track"
    :> ReqBody '[JSON] TrackTripReq
    :> Post '[JSON] TrackTripRes

trackTripAPI :: Proxy TrackTripAPI
trackTripAPI = Proxy

trackTrip :: TrackTripReq -> EulerClient ()
trackTrip req =
  void $ client trackTripAPI req

type CancelAPI =
  "cancel"
    :> ReqBody '[JSON] Cancel.CancelReq
    :> Post '[JSON] Cancel.CancelRes

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

cancel :: Cancel.CancelReq -> EulerClient ()
cancel req =
  void $ client cancelAPI req

type StatusAPI =
  "status"
    :> ReqBody '[JSON] Status.StatusReq
    :> Post '[JSON] Status.StatusRes

statusAPI :: Proxy StatusAPI
statusAPI = Proxy

status :: Status.StatusReq -> EulerClient ()
status req =
  void $ client statusAPI req
