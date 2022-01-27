module Types.API.Confirm where

import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import Domain.Types.RideBooking (RideBooking)
import EulerHS.Prelude

newtype ConfirmRes = ConfirmRes
  { bookingId :: Id RideBooking
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)
