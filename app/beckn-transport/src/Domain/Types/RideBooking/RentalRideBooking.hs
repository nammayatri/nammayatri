module Domain.Types.RideBooking.RentalRideBooking where

import Beckn.Types.Id
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import qualified Domain.Types.RideBooking as DRideBooking
import EulerHS.Prelude hiding (id)

-- Not used in business logic, only for Tabular
data RentalRideBooking = RentalRideBooking
  { rideBookingId :: Id DRideBooking.RideBooking,
    rentalFarePolicyId :: Id DRentalFP.RentalFarePolicy
  }
  deriving (Generic, Show, Eq)

mkRentalRideBooking :: Id DRideBooking.RideBooking -> DRideBooking.RentalRideBookingDetails -> RentalRideBooking
mkRentalRideBooking rideBookingId DRideBooking.RentalRideBookingDetails {..} = RentalRideBooking {..}
