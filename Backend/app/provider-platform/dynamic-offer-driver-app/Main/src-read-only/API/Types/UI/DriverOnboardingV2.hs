{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverOnboardingV2 where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.VehicleServiceTier
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data DocumentVerificationConfigAPIEntity = DocumentVerificationConfigAPIEntity
  { checkExpiry :: Kernel.Prelude.Bool,
    checkExtraction :: Kernel.Prelude.Bool,
    dependencyDocumentType :: [Domain.Types.DocumentVerificationConfig.DocumentType],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disableWarning :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    isDisabled :: Kernel.Prelude.Bool,
    isHidden :: Kernel.Prelude.Bool,
    isMandatory :: Kernel.Prelude.Bool,
    rcNumberPrefixList :: [Kernel.Prelude.Text],
    title :: Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DocumentVerificationConfigList = DocumentVerificationConfigList
  { autos :: Kernel.Prelude.Maybe [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity],
    bikes :: Kernel.Prelude.Maybe [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity],
    cabs :: Kernel.Prelude.Maybe [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverVehicleServiceTier = DriverVehicleServiceTier
  { airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    isDefault :: Kernel.Prelude.Bool,
    isSelected :: Kernel.Prelude.Bool,
    longDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    luggageCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    name :: Kernel.Prelude.Text,
    seatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    serviceTierType :: Domain.Types.VehicleServiceTier.ServiceTierType,
    shortDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype DriverVehicleServiceTiers = DriverVehicleServiceTiers {tiers :: [API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTier]} deriving (Generic, ToJSON, FromJSON, ToSchema)
