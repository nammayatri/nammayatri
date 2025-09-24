{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BackgroundVerification where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Tools.Beam.UtilsTH

data BackgroundVerificationT f = BackgroundVerificationT
  { candidateId :: (B.C f Kernel.Prelude.Text),
    driverId :: (B.C f Kernel.Prelude.Text),
    expiresAt :: (B.C f Kernel.Prelude.UTCTime),
    invitationId :: (B.C f Kernel.Prelude.Text),
    invitationStatus :: (B.C f Kernel.Types.Documents.VerificationStatus),
    invitationUrl :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    reportId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    reportStatus :: (B.C f Kernel.Types.Documents.VerificationStatus),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table BackgroundVerificationT where
  data PrimaryKey BackgroundVerificationT f = BackgroundVerificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BackgroundVerificationId . driverId

type BackgroundVerification = BackgroundVerificationT Identity

$(enableKVPG (''BackgroundVerificationT) [('driverId)] [])

$(mkTableInstances (''BackgroundVerificationT) "background_verification")
