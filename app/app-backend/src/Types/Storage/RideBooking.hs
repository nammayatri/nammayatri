module Types.Storage.RideBooking where

import EulerHS.Prelude
import Data.OpenApi (ToSchema)

data RideBookingStatus = NEW | CONFIRMED | TRIP_ASSIGNED | COMPLETED | CANCELLED
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)