{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DocumentVerificationConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DocumentVerificationConfigT f = DocumentVerificationConfigT
  { allowLicenseTransfer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    applicableTo :: B.C f (Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentApplicableType),
    checkExpiry :: B.C f Kernel.Prelude.Bool,
    checkExtraction :: B.C f Kernel.Prelude.Bool,
    dependencyDocumentType :: B.C f [Domain.Types.DocumentVerificationConfig.DocumentType],
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    disableWarning :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    doStrictVerifcation :: B.C f Kernel.Prelude.Bool,
    documentCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentCategory),
    documentFieldsJSON :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    documentType :: B.C f Domain.Types.DocumentVerificationConfig.DocumentType,
    filterForOldApks :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isDefaultEnabledOnManualVerification :: B.C f Kernel.Prelude.Bool,
    isDisabled :: B.C f Kernel.Prelude.Bool,
    isHidden :: B.C f Kernel.Prelude.Bool,
    isImageValidationRequired :: B.C f Kernel.Prelude.Bool,
    isMandatory :: B.C f Kernel.Prelude.Bool,
    isMandatoryForEnabling :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxRetryCount :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    order :: B.C f Kernel.Prelude.Int,
    rcNumberPrefixList :: B.C f [Kernel.Prelude.Text],
    supportedVehicleClassesJSON :: B.C f Data.Aeson.Value,
    title :: B.C f Kernel.Prelude.Text,
    vehicleCategory :: B.C f Domain.Types.VehicleCategory.VehicleCategory,
    vehicleClassCheckType :: B.C f Domain.Types.DocumentVerificationConfig.VehicleClassCheckType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DocumentVerificationConfigT where
  data PrimaryKey DocumentVerificationConfigT f
    = DocumentVerificationConfigId (B.C f Domain.Types.DocumentVerificationConfig.DocumentType) (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.VehicleCategory.VehicleCategory)
    deriving (Generic, B.Beamable)
  primaryKey = DocumentVerificationConfigId <$> documentType <*> merchantOperatingCityId <*> vehicleCategory

type DocumentVerificationConfig = DocumentVerificationConfigT Identity

$(enableKVPG ''DocumentVerificationConfigT ['documentType, 'merchantOperatingCityId, 'vehicleCategory] [])

$(mkTableInstances ''DocumentVerificationConfigT "document_verification_config")
