{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverOnboardingV2 where

import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
import qualified Domain.Types.Common
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DriverInformation
import qualified Domain.Types.DriverPanCard
import qualified Domain.Types.FareParameters
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.Image
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleCategory
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
    transactionId :: Kernel.Prelude.Text,
    validationStatus :: ValidationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AirConditionedTier = AirConditionedTier
  { isWorking :: Kernel.Prelude.Bool,
    restrictionMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    usageRestrictionType :: Domain.Types.DriverInformation.AirConditionedRestrictionType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BankAccountLinkResp = BankAccountLinkResp {accountLink :: Servant.Client.Core.BaseUrl, accountUrlExpiry :: Kernel.Prelude.UTCTime, chargesEnabled :: Kernel.Prelude.Bool, detailsSubmitted :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BankAccountResp = BankAccountResp {chargesEnabled :: Kernel.Prelude.Bool, detailsSubmitted :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommonDocumentReq = CommonDocumentReq
  { documentData :: Kernel.Prelude.Text,
    documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    imageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentVerificationConfigAPIEntity = DocumentVerificationConfigAPIEntity
  { applicableTo :: Domain.Types.DocumentVerificationConfig.DocumentApplicableType,
    checkExpiry :: Kernel.Prelude.Bool,
    checkExtraction :: Kernel.Prelude.Bool,
    dependencyDocumentType :: [Domain.Types.DocumentVerificationConfig.DocumentType],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disableWarning :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentCategory :: Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentCategory,
    documentFields :: Kernel.Prelude.Maybe [Domain.Types.DocumentVerificationConfig.FieldInfo],
    documentFlowGrouping :: Domain.Types.DocumentVerificationConfig.DocumentFlowGrouping,
    documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    filterForOldApks :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDisabled :: Kernel.Prelude.Bool,
    isHidden :: Kernel.Prelude.Bool,
    isMandatory :: Kernel.Prelude.Bool,
    isMandatoryForEnabling :: Kernel.Prelude.Bool,
    rcNumberPrefixList :: [Kernel.Prelude.Text],
    title :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentVerificationConfigList = DocumentVerificationConfigList
  { ambulances :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    autos :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    bikes :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    boat :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    bus :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    cabs :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    trucks :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverGstinReq = DriverGstinReq
  { gstNumber :: Kernel.Prelude.Text,
    imageId1 :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    imageId2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    transactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    validationStatus :: Kernel.Prelude.Maybe ValidationStatus,
    verifiedBy :: Kernel.Prelude.Maybe Domain.Types.DriverPanCard.VerifiedBy
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverPanReq = DriverPanReq
  { consent :: Kernel.Prelude.Bool,
    consentTimestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    dateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    docType :: Kernel.Prelude.Maybe Domain.Types.DriverPanCard.PanType,
    imageId1 :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    imageId2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    nameOnCard :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nameOnGovtDB :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panNumber :: Kernel.Prelude.Text,
    transactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    validationStatus :: Kernel.Prelude.Maybe ValidationStatus,
    verifiedBy :: Kernel.Prelude.Maybe Domain.Types.DriverPanCard.VerifiedBy
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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
    serviceTierType :: Domain.Types.Common.ServiceTierType,
    shortDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverVehicleServiceTiers = DriverVehicleServiceTiers
  { airConditioned :: Kernel.Prelude.Maybe AirConditionedTier,
    canSwitchToInterCity :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    canSwitchToIntraCity :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    canSwitchToRental :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    tiers :: [DriverVehicleServiceTier]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FarePolicyHour
  = Peak
  | NonPeak
  | Night
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetRCListRes = FleetRCListRes {rcs :: [Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate.LinkedRC]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype GetLiveSelfieResp = GetLiveSelfieResp {image :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data HVSdkCallLogReq = HVSdkCallLogReq
  { callbackResponse :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    docType :: Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentType,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hvFlowId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    txnId :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LinkToFleetReq = LinkToFleetReq
  { fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    isRevoke :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    onboardingVehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    requestReason :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RateCardItem = RateCardItem {price :: Kernel.Types.Common.Money, priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity, title :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RateCardResp = RateCardResp
  { fareParams :: Domain.Types.FareParameters.FareParameters,
    farePolicyHour :: FarePolicyHour,
    farePolicyId :: Kernel.Types.Id.Id Domain.Types.FarePolicy.FarePolicy,
    perKmRate :: Kernel.Types.Common.PriceAPIEntity,
    perMinuteRate :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    rateCardItems :: [RateCardItem],
    serviceTierType :: Domain.Types.Common.ServiceTierType,
    totalFare :: Kernel.Types.Common.PriceAPIEntity,
    tripCategory :: Domain.Types.Common.TripCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SSNReq = SSNReq {ssn :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype UpdateAirConditionUpdateRequest = UpdateAirConditionUpdateRequest {isAirConditioned :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ValidationStatus
  = APPROVED
  | DECLINED
  | AUTO_APPROVED
  | AUTO_DECLINED
  | NEEDS_REVIEW
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehiclePhotosResp = VehiclePhotosResp
  { back :: [Kernel.Prelude.Text],
    backInterior :: [Kernel.Prelude.Text],
    front :: [Kernel.Prelude.Text],
    frontInterior :: [Kernel.Prelude.Text],
    left :: [Kernel.Prelude.Text],
    odometer :: [Kernel.Prelude.Text],
    right :: [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
