module Domain.Action.Internal.DriverReachedDestination where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.VehicleVariant
import Environment
import EulerHS.Prelude
import Kernel.External.Maps.Types
import Kernel.Types.APISuccess
import Kernel.Types.Id

data DriverReachedDestinationReq = DriverReachedDestinationReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    location :: LatLong,
    vehicleVariant :: VehicleVariant
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

driverReachedDestination :: DriverReachedDestinationReq -> Flow APISuccess
driverReachedDestination _ =
  -- End The TripTransaction with Notification for Bus VehicleCategory
  pure Success
