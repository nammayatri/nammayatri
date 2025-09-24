{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Image where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentVerificationConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Tools.Beam.UtilsTH
import qualified Tools.Error

data ImageT f = ImageT
  { documentExpiry :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
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
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table ImageT where
  data PrimaryKey ImageT f = ImageId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ImageId . id

type Image = ImageT Identity

$(enableKVPG (''ImageT) [('id)] [[('personId)]])

$(mkTableInstances (''ImageT) "image")
