{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DocumentVerificationConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DocumentVerificationConfigT f = DocumentVerificationConfigT
  { checkExpiry :: B.C f Kernel.Prelude.Bool,
    checkExtraction :: B.C f Kernel.Prelude.Bool,
    dependencyDocumentType :: B.C f [Domain.Types.DocumentVerificationConfig.DocumentType],
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    disableWarning :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    doStrictVerifcation :: B.C f Kernel.Prelude.Bool,
    documentType :: B.C f Domain.Types.DocumentVerificationConfig.DocumentType,
    isDefaultEnabledOnManualVerification :: B.C f Kernel.Prelude.Bool,
    isDisabled :: B.C f Kernel.Prelude.Bool,
    isHidden :: B.C f Kernel.Prelude.Bool,
    isImageValidationRequired :: B.C f Kernel.Prelude.Bool,
    isMandatory :: B.C f Kernel.Prelude.Bool,
    maxRetryCount :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    rcNumberPrefixList :: B.C f [Kernel.Prelude.Text],
    supportedVehicleClassesJSON :: B.C f Data.Aeson.Value,
    title :: B.C f Kernel.Prelude.Text,
    vehicleCategory :: B.C f Domain.Types.Vehicle.Category,
    vehicleClassCheckType :: B.C f Domain.Types.DocumentVerificationConfig.VehicleClassCheckType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DocumentVerificationConfigT where
  data PrimaryKey DocumentVerificationConfigT f
    = DocumentVerificationConfigId (B.C f Domain.Types.DocumentVerificationConfig.DocumentType) (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.Vehicle.Category)
    deriving (Generic, B.Beamable)
  primaryKey = DocumentVerificationConfigId <$> documentType <*> merchantOperatingCityId <*> vehicleCategory

type DocumentVerificationConfig = DocumentVerificationConfigT Identity

$(enableKVPG ''DocumentVerificationConfigT ['documentType, 'merchantOperatingCityId, 'vehicleCategory] [])

$(mkTableInstances ''DocumentVerificationConfigT "document_verification_config")
