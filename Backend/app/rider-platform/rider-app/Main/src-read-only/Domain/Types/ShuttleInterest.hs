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
import qualified Domain.Types.PassOrganization
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ShuttleInterest = ShuttleInterest
  { companyDomain :: Kernel.Prelude.Text,
    createdAt :: Data.Time.UTCTime,
    daysOfWeek :: [Data.Time.Calendar.DayOfWeek],
    dropAddress :: Kernel.Prelude.Text,
    dropLat :: Kernel.Prelude.Double,
    dropLon :: Kernel.Prelude.Double,
    dropTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    passOrganizationId :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    pickupAddress :: Kernel.Prelude.Text,
    pickupLat :: Kernel.Prelude.Double,
    pickupLon :: Kernel.Prelude.Double,
    pickupTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    tripType :: Domain.Types.ShuttleInterest.ShuttleTripType,
    updatedAt :: Data.Time.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ShuttleTripType
  = TO_OFFICE
  | TO_HOME
  | ROUND_TRIP
  deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''ShuttleTripType))
