{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetOwnerDocumentVerificationConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentOnboardingStage
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetOwnerDocumentVerificationConfigT f = FleetOwnerDocumentVerificationConfigT
  { checkExpiry :: (B.C f Kernel.Prelude.Bool),
    checkExtraction :: (B.C f Kernel.Prelude.Bool),
    dependencyDocumentType :: (B.C f [Domain.Types.DocumentVerificationConfig.DocumentType]),
    description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    disableWarning :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    doStrictVerifcation :: (B.C f Kernel.Prelude.Bool),
    documentCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentCategory)),
    documentFieldsJSON :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    documentOnboardingStage :: (B.C f Domain.Types.DocumentOnboardingStage.DocumentOnboardingStage),
    documentType :: (B.C f Domain.Types.DocumentVerificationConfig.DocumentType),
    isDefaultEnabledOnManualVerification :: (B.C f Kernel.Prelude.Bool),
    isDisabled :: (B.C f Kernel.Prelude.Bool),
    isHidden :: (B.C f Kernel.Prelude.Bool),
    isImageValidationRequired :: (B.C f Kernel.Prelude.Bool),
    isMandatory :: (B.C f Kernel.Prelude.Bool),
    isMandatoryForEnabling :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    markImageValidOnValidationSkip :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    maxRetryCount :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    onlyImageVerificationStatusLookupRequired :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    order :: (B.C f Kernel.Prelude.Int),
    role :: (B.C f Domain.Types.Person.Role),
    rolesAllowedToUploadDocumentText :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    title :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetOwnerDocumentVerificationConfigT where
  data PrimaryKey FleetOwnerDocumentVerificationConfigT f
    = FleetOwnerDocumentVerificationConfigId
        (B.C f Domain.Types.DocumentOnboardingStage.DocumentOnboardingStage)
        (B.C f Domain.Types.DocumentVerificationConfig.DocumentType)
        (B.C f Kernel.Prelude.Text)
        (B.C f Domain.Types.Person.Role)
    deriving (Generic, B.Beamable)
  primaryKey = FleetOwnerDocumentVerificationConfigId <$> documentOnboardingStage <*> documentType <*> merchantOperatingCityId <*> role

type FleetOwnerDocumentVerificationConfig = FleetOwnerDocumentVerificationConfigT Identity

$(enableKVPG (''FleetOwnerDocumentVerificationConfigT) [('documentOnboardingStage), ('documentType), ('merchantOperatingCityId), ('role)] [])

$(mkTableInstances (''FleetOwnerDocumentVerificationConfigT) "fleet_owner_document_verification_config")
