{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetOnwerDocumentVerificationConfig where

import qualified Database.Beam as B
import qualified Domain.Types.DocumentVerificationConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetOnwerDocumentVerificationConfigT f = FleetOnwerDocumentVerificationConfigT
  { checkExpiry :: (B.C f Kernel.Prelude.Bool),
    checkExtraction :: (B.C f Kernel.Prelude.Bool),
    dependencyDocumentType :: (B.C f [Domain.Types.DocumentVerificationConfig.DocumentType]),
    description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    disableWarning :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    documentType :: (B.C f Domain.Types.DocumentVerificationConfig.DocumentType),
    isDisabled :: (B.C f Kernel.Prelude.Bool),
    isHidden :: (B.C f Kernel.Prelude.Bool),
    isMandatory :: (B.C f Kernel.Prelude.Bool),
    maxRetryCount :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    order :: (B.C f Kernel.Prelude.Int),
    title :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetOnwerDocumentVerificationConfigT where
  data PrimaryKey FleetOnwerDocumentVerificationConfigT f
    = FleetOnwerDocumentVerificationConfigId (B.C f Domain.Types.DocumentVerificationConfig.DocumentType) (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = FleetOnwerDocumentVerificationConfigId <$> documentType <*> merchantOperatingCityId

type FleetOnwerDocumentVerificationConfig = FleetOnwerDocumentVerificationConfigT Identity

$(enableKVPG (''FleetOnwerDocumentVerificationConfigT) [('documentType), ('merchantOperatingCityId)] [])

$(mkTableInstances (''FleetOnwerDocumentVerificationConfigT) "fleet_owner_document_verification_config")
