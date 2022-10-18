module Domain.Types.FarePolicy.FareBreakup where

import Beckn.Prelude
import Beckn.Types.Common (Centesimal)
import Beckn.Types.Id
import Domain.Types.Booking.Type

data FareBreakup = FareBreakup
  { id :: Id FareBreakup,
    bookingId :: Id Booking,
    description :: Text,
    amount :: Centesimal
  }
  deriving (Show)

data FareBreakupAPIEntity = FareBreakupAPIEntity
  { description :: Text,
    amount :: Centesimal
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

mkFareBreakupAPIEntity :: FareBreakup -> FareBreakupAPIEntity
mkFareBreakupAPIEntity FareBreakup {..} = FareBreakupAPIEntity {..}
