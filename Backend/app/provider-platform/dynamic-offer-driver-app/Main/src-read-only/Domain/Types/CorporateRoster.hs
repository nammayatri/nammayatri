{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateRoster where

import Data.Aeson
import Data.Time.Calendar (Day)
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateRoster = CorporateRoster
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster,
    corporateEntityId :: Kernel.Prelude.Text,
    corporateEmployeeId :: Kernel.Prelude.Text,
    corporateShiftId :: Kernel.Prelude.Text,
    corporateRouteId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rosterDate :: Data.Time.Calendar.Day,
    attendanceStatus :: CorporateAttendanceStatus,
    bookingId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    confirmedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CorporateAttendanceStatus = SCHEDULED | CONFIRMED | ATT_ON_LEAVE | NO_SHOW | COMPLETED | CANCELLED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporateAttendanceStatus)
