module Types.API.Confirm where

import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.Storage.OldRide (Ride)

newtype ConfirmRes = ConfirmRes
  { bookingId :: Id Ride
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)
