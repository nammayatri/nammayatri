{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.CorporateCommute where

import Data.OpenApi (ToSchema)
import Data.Time.Calendar (Day)
import qualified Domain.Types.CorporateRoster
import qualified Domain.Types.CorporateRoute
import qualified Domain.Types.CorporateShift
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data CorporateShiftDriverResp = CorporateShiftDriverResp
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift,
    name :: Kernel.Prelude.Text,
    pickupWindowStart :: Kernel.Prelude.Text,
    pickupWindowEnd :: Kernel.Prelude.Text,
    dropWindowStart :: Kernel.Prelude.Text,
    dropWindowEnd :: Kernel.Prelude.Text,
    vehicleTier :: Kernel.Prelude.Text,
    isNightShift :: Kernel.Prelude.Bool,
    maxOccupancy :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateRouteDriverResp = CorporateRouteDriverResp
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateRoute.CorporateRoute,
    routeCode :: Kernel.Prelude.Text,
    direction :: Domain.Types.CorporateRoute.CorporateRouteDirection,
    estimatedDurationMinutes :: Kernel.Prelude.Int,
    estimatedDistanceMeters :: Kernel.Prelude.Int,
    maxCapacity :: Kernel.Prelude.Int,
    status :: Domain.Types.CorporateRoute.CorporateRouteStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateRosterDriverResp = CorporateRosterDriverResp
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster,
    rosterDate :: Data.Time.Calendar.Day,
    shiftName :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupTime :: Kernel.Prelude.Text,
    attendanceStatus :: Domain.Types.CorporateRoster.CorporateAttendanceStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AcceptCorporateRideReq = AcceptCorporateRideReq
  { rosterId :: Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CorporateEarningsSummary = CorporateEarningsSummary
  { totalTrips :: Kernel.Prelude.Int,
    totalEarnings :: Kernel.Types.Common.HighPrecMoney,
    periodStart :: Kernel.Prelude.UTCTime,
    periodEnd :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
