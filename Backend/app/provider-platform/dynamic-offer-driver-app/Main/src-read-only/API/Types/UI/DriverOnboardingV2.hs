{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverOnboardingV2 where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Common
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DriverInformation
import qualified Domain.Types.DriverPanCard
import qualified Domain.Types.Image
import qualified Domain.Types.ServiceTierType
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import qualified Servant.Client.Core
import Tools.Auth

data AadhaarCardReq = AadhaarCardReq
  { aadhaarBackImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    aadhaarFrontImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    consent :: Kernel.Prelude.Bool,
    consentTimestamp :: Kernel.Prelude.UTCTime,
    dateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maskedAadhaarNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nameOnCard :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    validationStatus :: API.Types.UI.DriverOnboardingV2.ValidationStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AirConditionedTier = AirConditionedTier
  { isWorking :: Kernel.Prelude.Bool,
    restrictionMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    usageRestrictionType :: Domain.Types.DriverInformation.AirConditionedRestrictionType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data BankAccountLinkResp = BankAccountLinkResp {accountLink :: Servant.Client.Core.BaseUrl, accountUrlExpiry :: Kernel.Prelude.UTCTime, chargesEnabled :: Kernel.Prelude.Bool, detailsSubmitted :: Kernel.Prelude.Bool}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data BankAccountResp = BankAccountResp {chargesEnabled :: Kernel.Prelude.Bool, detailsSubmitted :: Kernel.Prelude.Bool} deriving (Generic, ToJSON, FromJSON, ToSchema)

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
  { ambulances :: Kernel.Prelude.Maybe [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity],
    autos :: Kernel.Prelude.Maybe [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity],
    bikes :: Kernel.Prelude.Maybe [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity],
    cabs :: Kernel.Prelude.Maybe [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverPanReq = DriverPanReq
  { consent :: Kernel.Prelude.Bool,
    consentTimestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    dateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    imageId1 :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    imageId2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    nameOnCard :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panNumber :: Kernel.Prelude.Text,
    validationStatus :: Kernel.Prelude.Maybe API.Types.UI.DriverOnboardingV2.ValidationStatus,
    verifiedBy :: Kernel.Prelude.Maybe Domain.Types.DriverPanCard.VerifiedBy
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverVehicleServiceTier = DriverVehicleServiceTier
  { airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    isDefault :: Kernel.Prelude.Bool,
    isSelected :: Kernel.Prelude.Bool,
    isUsageRestricted :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    longDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    luggageCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    name :: Kernel.Prelude.Text,
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    seatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    serviceTierType :: Domain.Types.ServiceTierType.ServiceTierType,
    shortDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverVehicleServiceTiers = DriverVehicleServiceTiers
  { airConditioned :: Kernel.Prelude.Maybe API.Types.UI.DriverOnboardingV2.AirConditionedTier,
    canSwitchToInterCity :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    canSwitchToRental :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    tiers :: [API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTier]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FarePolicyHour = Peak | NonPeak | Night deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

newtype GetLiveSelfieResp = GetLiveSelfieResp {image :: Kernel.Prelude.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

data RateCardItem = RateCardItem {price :: Kernel.Types.Common.Money, priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity, title :: Kernel.Prelude.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data RateCardResp = RateCardResp
  { farePolicyHour :: API.Types.UI.DriverOnboardingV2.FarePolicyHour,
    perKmRate :: Kernel.Types.Common.PriceAPIEntity,
    perMinuteRate :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    rateCardItems :: [API.Types.UI.DriverOnboardingV2.RateCardItem],
    serviceTierType :: Domain.Types.ServiceTierType.ServiceTierType,
    totalFare :: Kernel.Types.Common.PriceAPIEntity,
    tripCategory :: Domain.Types.Common.TripCategory
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SSNReq = SSNReq {ssn :: Kernel.Prelude.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype UpdateAirConditionUpdateRequest = UpdateAirConditionUpdateRequest {isAirConditioned :: Kernel.Prelude.Bool} deriving (Generic, ToJSON, FromJSON, ToSchema)

data ValidationStatus = APPROVED | DECLINED | AUTO_APPROVED | AUTO_DECLINED | NEEDS_REVIEW deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, Read)

{-
	DSL Source Link: file://./../../../../spec/API/DriverOnboarding.yaml
-}
