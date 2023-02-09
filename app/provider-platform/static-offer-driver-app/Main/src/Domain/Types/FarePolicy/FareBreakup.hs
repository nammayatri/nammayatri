module Domain.Types.FarePolicy.FareBreakup where

import Domain.Types.Booking.Type
import Kernel.Prelude
import Kernel.Types.Common (Centesimal)
import Kernel.Types.Id

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
