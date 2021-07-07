module Types.API.Track where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import EulerHS.Prelude
import Types.Storage.ProductInstance (ProductInstance)

newtype TrackTripReq = TrackTripReq
  { rideId :: Id ProductInstance
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type TrackTripRes = APISuccess
