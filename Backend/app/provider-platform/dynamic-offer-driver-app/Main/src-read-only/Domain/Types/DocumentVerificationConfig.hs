{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DocumentVerificationConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DocumentVerificationConfig = DocumentVerificationConfig
  { checkExpiry :: Kernel.Prelude.Bool,
    checkExtraction :: Kernel.Prelude.Bool,
    dependencyDocumentType :: [Domain.Types.DocumentVerificationConfig.DocumentType],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disableWarning :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    doStrictVerifcation :: Kernel.Prelude.Bool,
    documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    filterForOldApks :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDefaultEnabledOnManualVerification :: Kernel.Prelude.Bool,
    isDisabled :: Kernel.Prelude.Bool,
    isHidden :: Kernel.Prelude.Bool,
    isImageValidationRequired :: Kernel.Prelude.Bool,
    isMandatory :: Kernel.Prelude.Bool,
    maxRetryCount :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    order :: Kernel.Prelude.Int,
    rcNumberPrefixList :: [Kernel.Prelude.Text],
    supportedVehicleClasses :: Domain.Types.DocumentVerificationConfig.SupportedVehicleClasses,
    title :: Kernel.Prelude.Text,
    vehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory,
    vehicleClassCheckType :: Domain.Types.DocumentVerificationConfig.VehicleClassCheckType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DocumentType
  = DriverLicense
  | VehicleRegistrationCertificate
  | Permissions
  | SubscriptionPlan
  | ProfilePhoto
  | AadhaarCard
  | PanCard
  | VehiclePermit
  | VehicleFitnessCertificate
  | VehicleInsurance
  | VehiclePUC
  | ProfileDetails
  | SocialSecurityNumber
  | VehicleInspectionForm
  | UploadProfile
  | GSTCertificate
  | BackgroundVerification
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SupportedVehicleClasses
  = DLValidClasses [Kernel.Prelude.Text]
  | RCValidClasses [Domain.Types.DocumentVerificationConfig.VehicleClassVariantMap]
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data VehicleClassCheckType = Infix | Prefix | Suffix deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data VehicleClassVariantMap = VehicleClassVariantMap
  { bodyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    manufacturer :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    manufacturerModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    reviewRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    vehicleCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleClass :: Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DocumentType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SupportedVehicleClasses)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''VehicleClassCheckType)
