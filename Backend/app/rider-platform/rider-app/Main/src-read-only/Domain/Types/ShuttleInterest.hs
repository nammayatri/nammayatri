{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ShuttleInterest (module Domain.Types.ShuttleInterest, module ReExport) where

import Data.Aeson
import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import Domain.Types.Extra.ShuttleInterest as ReExport
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ShuttleInterest = ShuttleInterest
  { acPreference :: Kernel.Prelude.Maybe Domain.Types.ShuttleInterest.ShuttleAcPreference,
    companyDomain :: Kernel.Prelude.Text,
    createdAt :: Data.Time.UTCTime,
    daysOfWeek :: [Data.Time.Calendar.DayOfWeek],
    dropAddress :: Kernel.Prelude.Text,
    dropLat :: Kernel.Prelude.Double,
    dropLon :: Kernel.Prelude.Double,
    dropTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    maxWalkingDistanceMeters :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    pickupAddress :: Kernel.Prelude.Text,
    pickupLat :: Kernel.Prelude.Double,
    pickupLon :: Kernel.Prelude.Double,
    pickupTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    shiftEndTime :: Data.Time.LocalTime.TimeOfDay,
    shiftStartTime :: Data.Time.LocalTime.TimeOfDay,
    timeFlexibilityMinutes :: Kernel.Prelude.Int,
    tripType :: Domain.Types.ShuttleInterest.ShuttleTripType,
    updatedAt :: Data.Time.UTCTime,
    vehicleSizePreference :: Kernel.Prelude.Maybe Domain.Types.ShuttleInterest.ShuttleVehicleSizePreference
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ShuttleAcPreference
  = AC_PREFERRED
  | NON_AC_PREFERRED
  | AC_ANY
  deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

data ShuttleTripType
  = TO_OFFICE
  | TO_HOME
  | ROUND_TRIP
  deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

data ShuttleVehicleSizePreference
  = CAB
  | TEMPO_TRAVELLER
  | MINI_BUS
  | BUS
  | SIZE_ANY
  deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''ShuttleTripType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''ShuttleAcPreference))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''ShuttleVehicleSizePreference))
