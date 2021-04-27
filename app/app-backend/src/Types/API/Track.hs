module Types.API.Track where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Types.Storage.ProductInstance (ProductInstance)
import EulerHS.Prelude

newtype TrackTripReq = TrackTripReq
  { rideId :: Id ProductInstance
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type TrackTripRes = APISuccess
