{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FleetOwnerDocumentVerificationConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Person
import qualified Database.Beam as B



data FleetOwnerDocumentVerificationConfigT f
    = FleetOwnerDocumentVerificationConfigT {checkExpiry :: (B.C f Kernel.Prelude.Bool),
                                             checkExtraction :: (B.C f Kernel.Prelude.Bool),
                                             dependencyDocumentType :: (B.C f [Domain.Types.DocumentVerificationConfig.DocumentType]),
                                             description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                             disableWarning :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                             doStrictVerifcation :: (B.C f Kernel.Prelude.Bool),
                                             documentCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentCategory)),
                                             documentType :: (B.C f Domain.Types.DocumentVerificationConfig.DocumentType),
                                             isDefaultEnabledOnManualVerification :: (B.C f Kernel.Prelude.Bool),
                                             isDisabled :: (B.C f Kernel.Prelude.Bool),
                                             isHidden :: (B.C f Kernel.Prelude.Bool),
                                             isImageValidationRequired :: (B.C f Kernel.Prelude.Bool),
                                             isMandatory :: (B.C f Kernel.Prelude.Bool),
                                             maxRetryCount :: (B.C f Kernel.Prelude.Int),
                                             merchantId :: (B.C f Kernel.Prelude.Text),
                                             merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                             order :: (B.C f Kernel.Prelude.Int),
                                             role :: (B.C f Domain.Types.Person.Role),
                                             title :: (B.C f Kernel.Prelude.Text),
                                             createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                             updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FleetOwnerDocumentVerificationConfigT
    where data PrimaryKey FleetOwnerDocumentVerificationConfigT f
              = FleetOwnerDocumentVerificationConfigId (B.C f Domain.Types.DocumentVerificationConfig.DocumentType) (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.Person.Role)
              deriving (Generic, B.Beamable)
          primaryKey = FleetOwnerDocumentVerificationConfigId <$> documentType <*> merchantOperatingCityId <*> role
type FleetOwnerDocumentVerificationConfig = FleetOwnerDocumentVerificationConfigT Identity

$(enableKVPG (''FleetOwnerDocumentVerificationConfigT) [('documentType), ('merchantOperatingCityId), ('role)] [])

$(mkTableInstances (''FleetOwnerDocumentVerificationConfigT) "fleet_owner_document_verification_config")

