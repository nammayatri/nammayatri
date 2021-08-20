module Types.API.Confirm where

import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.Storage.RideBooking (RideBooking)

newtype ConfirmRes = ConfirmRes
  { bookingId :: Id RideBooking
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)
