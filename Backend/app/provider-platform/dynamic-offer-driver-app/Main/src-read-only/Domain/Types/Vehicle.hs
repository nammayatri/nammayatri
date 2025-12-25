{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Vehicle where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.UpgradedTier
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Vehicle = Vehicle
  { airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    category :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    color :: Kernel.Prelude.Text,
    downgradeReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    energyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    luggageCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    mYManufacturing :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    make :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    model :: Kernel.Prelude.Text,
    oxygen :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    registrationCategory :: Kernel.Prelude.Maybe Domain.Types.Vehicle.RegistrationCategory,
    registrationNo :: Kernel.Prelude.Text,
    ruleBasedUpgradeTiers :: Kernel.Prelude.Maybe [Domain.Types.UpgradedTier.UpgradedTier],
    selectedServiceTiers :: [Domain.Types.Common.ServiceTierType],
    size :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    variant :: Domain.Types.VehicleVariant.VehicleVariant,
    vehicleClass :: Kernel.Prelude.Text,
    vehicleImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile),
    vehicleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    vehicleTags :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    ventilator :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RegistrationCategory = COMMERCIAL | PERSONAL | OTHER | PUBLIC deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data VehicleAPIEntity = VehicleAPIEntity
  { capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    category :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    color :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    model :: Kernel.Prelude.Text,
    registrationNo :: Kernel.Prelude.Text,
    serviceTierType :: Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType,
    variant :: Domain.Types.VehicleVariant.VehicleVariant,
    vehicleImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile),
    vehicleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RegistrationCategory)

$(mkHttpInstancesForEnum ''RegistrationCategory)
