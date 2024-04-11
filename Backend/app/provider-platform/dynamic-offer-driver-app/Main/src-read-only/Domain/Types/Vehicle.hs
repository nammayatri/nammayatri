{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Vehicle where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.ServiceTierType
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Vehicle = Vehicle
  { airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    category :: Kernel.Prelude.Maybe Domain.Types.Vehicle.Category,
    color :: Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    energyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    luggageCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    make :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    model :: Kernel.Prelude.Text,
    registrationCategory :: Kernel.Prelude.Maybe Domain.Types.Vehicle.RegistrationCategory,
    registrationNo :: Kernel.Prelude.Text,
    selectedServiceTiers :: [Domain.Types.ServiceTierType.ServiceTierType],
    size :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    variant :: Domain.Types.Vehicle.Variant,
    vehicleClass :: Kernel.Prelude.Text,
    vehicleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Category = CAR | MOTORCYCLE | TRAIN | BUS | FLIGHT | AUTO_CATEGORY deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RegistrationCategory = COMMERCIAL | PERSONAL | OTHER | PUBLIC deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data Variant = SEDAN | SUV | HATCHBACK | AUTO_RICKSHAW | TAXI | TAXI_PLUS deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data VehicleAPIEntity = VehicleAPIEntity
  { capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    category :: Kernel.Prelude.Maybe Domain.Types.Vehicle.Category,
    color :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    model :: Kernel.Prelude.Text,
    registrationNo :: Kernel.Prelude.Text,
    serviceTierType :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    variant :: Domain.Types.Vehicle.Variant,
    vehicleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Category)

$(mkHttpInstancesForEnum ''Category)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RegistrationCategory)

$(mkHttpInstancesForEnum ''RegistrationCategory)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Variant)

$(mkHttpInstancesForEnum ''Variant)
