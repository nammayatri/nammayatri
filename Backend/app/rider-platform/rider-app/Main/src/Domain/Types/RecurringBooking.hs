module Domain.Types.RecurringBooking where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Calendar
  ( Day,
    DayOfWeek,
    dayOfWeek,
  )
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data RecurringBooking = RecurringBooking
  { id :: Id RecurringBooking,
    merchantId :: Id DMerchant.Merchant,
    scheduledDays :: Set DayOfWeek,
    startDate :: Day,
    endDate :: Maybe Day,
    pickupTime :: TimeOfDay,
    status :: Status,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    riderId :: Id DPerson.Person,
    fromLocation :: DLoc.BookingLocation,
    toLocation :: DLoc.BookingLocation,
    distance :: HighPrecMeters,
    vehicleVariant :: VehicleVariant
  }
  deriving (Generic, Show)

data Status
  = Active
  | Paused
  deriving (Generic, Show, Read)

scheduledDates :: RecurringBooking -> [Day]
scheduledDates booking =
  [booking.startDate ..]
    & takeWhile (\day -> maybe True (day <=) booking.endDate)
    & filter (\day -> dayOfWeek day `Set.member` booking.scheduledDays)
