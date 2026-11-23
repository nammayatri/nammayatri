{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.ShuttleInterest where

import Data.OpenApi (ToSchema)
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import qualified Domain.Types.ShuttleInterest
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data ShuttleEligibilityResp = ShuttleEligibilityResp
  { companyName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    eligible :: Kernel.Prelude.Bool,
    hasSubmittedInterest :: Kernel.Prelude.Bool,
    officeAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    officeLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    officeLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    workEmailVerified :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttleInterestReq = ShuttleInterestReq
  { acPreference :: Kernel.Prelude.Maybe Domain.Types.ShuttleInterest.ShuttleAcPreference,
    daysOfWeek :: [Data.Time.Calendar.DayOfWeek],
    dropAddress :: Kernel.Prelude.Text,
    dropLat :: Kernel.Prelude.Double,
    dropLon :: Kernel.Prelude.Double,
    dropTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    maxWalkingDistanceMeters :: Kernel.Prelude.Int,
    pickupAddress :: Kernel.Prelude.Text,
    pickupLat :: Kernel.Prelude.Double,
    pickupLon :: Kernel.Prelude.Double,
    pickupTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    shiftEndTime :: Data.Time.LocalTime.TimeOfDay,
    shiftStartTime :: Data.Time.LocalTime.TimeOfDay,
    timeFlexibilityMinutes :: Kernel.Prelude.Int,
    tripType :: Domain.Types.ShuttleInterest.ShuttleTripType,
    vehicleSizePreference :: Kernel.Prelude.Maybe Domain.Types.ShuttleInterest.ShuttleVehicleSizePreference
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
