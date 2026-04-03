{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Image where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Tools.Error
import qualified Domain.Types.DocumentVerificationConfig
import qualified Kernel.Types.Documents
import qualified Database.Beam as B



data ImageT f
    = ImageT {documentExpiry :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
              failureReason :: (B.C f (Kernel.Prelude.Maybe Tools.Error.DriverOnboardingError)),
              id :: (B.C f Kernel.Prelude.Text),
              imageType :: (B.C f Domain.Types.DocumentVerificationConfig.DocumentType),
              merchantId :: (B.C f Kernel.Prelude.Text),
              merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
              personId :: (B.C f Kernel.Prelude.Text),
              rcId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
              reviewerEmail :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
              s3Path :: (B.C f Kernel.Prelude.Text),
              verificationStatus :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Documents.VerificationStatus)),
              workflowTransactionId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
              createdAt :: (B.C f Kernel.Prelude.UTCTime),
              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table ImageT
    where data PrimaryKey ImageT f = ImageId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = ImageId . id
type Image = ImageT Identity

$(enableKVPG (''ImageT) [('id)] [[('personId)]])

$(mkTableInstances (''ImageT) "image")

