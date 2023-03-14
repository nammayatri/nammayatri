module Domain.Types.RecurringBooking where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Calendar (Day, DayOfWeek, dayOfWeek)
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.FarePolicy as DFarePolicy
import Kernel.Prelude
import Kernel.Types.Id

data SimpleRecurringBooking = SimpleRecurringBooking
  { id :: Id RecurringBooking,
    scheduledDays :: Set DayOfWeek,
    startDate :: Day,
    endDate :: Maybe Day,
    pickupTime :: TimeOfDay,
    status :: Status,
    providerId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    farePolicyId :: Id DFarePolicy.FarePolicy,
    fromLocationId :: Id DLoc.BookingLocation,
    toLocationId :: Id DLoc.BookingLocation
  }
  deriving (Generic, Show)

data RecurringBooking = RecurringBooking
  { id :: Id RecurringBooking,
    scheduledDays :: Set DayOfWeek,
    startDate :: Day,
    endDate :: Maybe Day,
    pickupTime :: TimeOfDay,
    status :: Status,
    providerId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    farePolicyId :: Id DFarePolicy.FarePolicy,
    fromLocation :: DLoc.BookingLocation,
    toLocation :: DLoc.BookingLocation
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
