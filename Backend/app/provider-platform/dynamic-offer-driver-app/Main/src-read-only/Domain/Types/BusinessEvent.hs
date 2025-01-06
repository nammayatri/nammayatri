{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BusinessEvent where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Domain.Types.VehicleVariant
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data BusinessEvent = BusinessEvent
  { bookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Booking.Booking),
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Driver),
    duration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    eventType :: Domain.Types.BusinessEvent.EventType,
    id :: Kernel.Types.Id.Id Domain.Types.BusinessEvent.BusinessEvent,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
    timeStamp :: Kernel.Prelude.UTCTime,
    vehicleVariant :: Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant,
    whenPoolWasComputed :: Kernel.Prelude.Maybe Domain.Types.BusinessEvent.WhenPoolWasComputed
  }
  deriving (Generic)

data EventType = DRIVER_IN_POOL | RIDE_COMMENCED | DRIVER_ASSIGNED | RIDE_CONFIRMED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data WhenPoolWasComputed = ON_SEARCH | ON_CONFIRM | ON_REALLOCATION deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''EventType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''WhenPoolWasComputed)
