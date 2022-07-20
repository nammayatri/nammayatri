module Domain.Types.FarePolicy.FareBreakup where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.Booking.Type

data FareBreakup = FareBreakup
  { id :: Id FareBreakup,
    rideBookingId :: Id RideBooking,
    description :: Text,
    amount :: Amount
  }
  deriving (Show)

data FareBreakupAPIEntity = FareBreakupAPIEntity
  { description :: Text,
    amount :: Amount
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

mkFareBreakupAPIEntity :: FareBreakup -> FareBreakupAPIEntity
mkFareBreakupAPIEntity FareBreakup {..} = FareBreakupAPIEntity {..}
