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
  { daysOfWeek :: [Data.Time.Calendar.DayOfWeek],
    dropAccuracyMeters :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    dropAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropLat :: Kernel.Prelude.Double,
    dropLocationSource :: Kernel.Prelude.Maybe Domain.Types.ShuttleInterest.ShuttleLocationSource,
    dropLon :: Kernel.Prelude.Double,
    dropTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    pickupAccuracyMeters :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    pickupAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupLat :: Kernel.Prelude.Double,
    pickupLocationSource :: Kernel.Prelude.Maybe Domain.Types.ShuttleInterest.ShuttleLocationSource,
    pickupLon :: Kernel.Prelude.Double,
    pickupTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    tripType :: Domain.Types.ShuttleInterest.ShuttleTripType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
