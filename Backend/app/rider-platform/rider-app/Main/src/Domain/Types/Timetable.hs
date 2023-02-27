module Domain.Types.Timetable where

import Data.Time.Calendar (Day)
import qualified Domain.Types.RecurringBooking as DRBooking
import Kernel.Prelude
import Kernel.Types.Id

data Status
  = Active
  | Cancelled
  deriving (Generic, Show, Read)

data Timetable = Timetable
  { id :: Id Timetable,
    recurringBookingId :: Id DRBooking.RecurringBooking,
    pickupDate :: Day,
    pickupTime :: TimeOfDay,
    status :: Status
  }
  deriving (Generic, Show)
