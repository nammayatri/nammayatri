module Domain.Types.BusinessEvent where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import Domain.Types.Ride (Ride)
import Domain.Types.RideBooking (RideBooking)
import Domain.Types.Vehicle (Variant)
import Types.App (Driver)

data BusinessEvent = BusinessEvent
  { id :: Id BusinessEvent,
    driverId :: Maybe (Id Driver),
    eventType :: EventType,
    timeStamp :: UTCTime,
    rideBookingId :: Maybe (Id RideBooking),
    whenPoolWasComputed :: Maybe WhenPoolWasComputed,
    vehicleVariant :: Maybe Variant,
    distance :: Maybe Meters,
    duration :: Maybe Seconds,
    rideId :: Maybe (Id Ride)
  }
  deriving (Generic)

data EventType = DRIVER_IN_POOL | RIDE_COMMENCED | DRIVER_ASSIGNED | RIDE_CONFIRMED
  deriving (Show, Eq, Read, Generic, ToSchema)

data WhenPoolWasComputed = ON_SEARCH | ON_CONFIRM | ON_REALLOCATION
  deriving (Show, Eq, Read, Generic, ToSchema)
