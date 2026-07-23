{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetOwnerDocumentVerificationConfig where

import Data.Aeson
import qualified Domain.Types.DocumentOnboardingStage
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetOwnerDocumentVerificationConfig = FleetOwnerDocumentVerificationConfig
  { checkExpiry :: Kernel.Prelude.Bool,
    checkExtraction :: Kernel.Prelude.Bool,
    dependencyDocumentType :: [Domain.Types.DocumentVerificationConfig.DocumentType],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disableWarning :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    doStrictVerifcation :: Kernel.Prelude.Bool,
    documentCategory :: Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentCategory,
    documentFields :: Kernel.Prelude.Maybe [Domain.Types.DocumentVerificationConfig.FieldInfo],
    documentOnboardingStage :: Kernel.Prelude.Maybe Domain.Types.DocumentOnboardingStage.DocumentOnboardingStage,
    documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    isDefaultEnabledOnManualVerification :: Kernel.Prelude.Bool,
    isDisabled :: Kernel.Prelude.Bool,
    isHidden :: Kernel.Prelude.Bool,
    isImageValidationRequired :: Kernel.Prelude.Bool,
    isMandatory :: Kernel.Prelude.Bool,
    isMandatoryForEnabling :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    markImageValidOnValidationSkip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    maxRetryCount :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    onlyImageVerificationStatusLookupRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    order :: Kernel.Prelude.Int,
    role :: Domain.Types.Person.Role,
    rolesAllowedToUploadDocument :: Kernel.Prelude.Maybe [Domain.Types.Person.Role],
    title :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
