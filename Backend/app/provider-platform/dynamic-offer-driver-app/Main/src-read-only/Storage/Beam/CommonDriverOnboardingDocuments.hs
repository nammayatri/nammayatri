{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.CommonDriverOnboardingDocuments where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.DocumentVerificationConfig
import qualified Kernel.Types.Documents
import qualified Database.Beam as B



data CommonDriverOnboardingDocumentsT f
    = CommonDriverOnboardingDocumentsT {documentData :: (B.C f Kernel.Prelude.Text),
                                        documentImageId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                        documentType :: (B.C f Domain.Types.DocumentVerificationConfig.DocumentType),
                                        driverId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                        id :: (B.C f Kernel.Prelude.Text),
                                        merchantId :: (B.C f Kernel.Prelude.Text),
                                        merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                        rejectReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                        verificationStatus :: (B.C f Kernel.Types.Documents.VerificationStatus),
                                        createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                        updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table CommonDriverOnboardingDocumentsT
    where data PrimaryKey CommonDriverOnboardingDocumentsT f = CommonDriverOnboardingDocumentsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = CommonDriverOnboardingDocumentsId . id
type CommonDriverOnboardingDocuments = CommonDriverOnboardingDocumentsT Identity

$(enableKVPG (''CommonDriverOnboardingDocumentsT) [('id)] [[('documentImageId)], [('driverId)]])

$(mkTableInstances (''CommonDriverOnboardingDocumentsT) "common_driver_onboarding_documents")

