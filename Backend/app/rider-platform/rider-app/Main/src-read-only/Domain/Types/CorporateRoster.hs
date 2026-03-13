{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateRoster (module Domain.Types.CorporateRoster, module ReExport) where

import Data.Aeson
import Data.Time.Calendar (Day)
import Domain.Types.Extra.CorporateRoster as ReExport
import qualified Domain.Types.Booking
import qualified Domain.Types.CorporateEmployee
import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.CorporateRoute
import qualified Domain.Types.CorporateRouteStop
import qualified Domain.Types.CorporateShift
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateRoster = CorporateRoster
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster,
    corporateEntityId :: Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity,
    corporateEmployeeId :: Kernel.Types.Id.Id Domain.Types.CorporateEmployee.CorporateEmployee,
    corporateShiftId :: Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift,
    corporateRouteId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.CorporateRoute.CorporateRoute),
    corporateRouteStopId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.CorporateRouteStop.CorporateRouteStop),
    rosterDate :: Data.Time.Calendar.Day,
    attendanceStatus :: Domain.Types.CorporateRoster.CorporateAttendanceStatus,
    bookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Booking.Booking),
    confirmedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CorporateAttendanceStatus = SCHEDULED | CONFIRMED | ATT_ON_LEAVE | NO_SHOW | COMPLETED | CANCELLED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporateAttendanceStatus)
