module Types.Storage.RideBooking where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data RideBookingStatus = NEW | CONFIRMED | TRIP_ASSIGNED | COMPLETED | CANCELLED
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)
