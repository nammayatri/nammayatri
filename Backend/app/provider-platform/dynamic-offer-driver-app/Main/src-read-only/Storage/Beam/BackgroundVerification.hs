{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BackgroundVerification where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import qualified Database.Beam as B



data BackgroundVerificationT f
    = BackgroundVerificationT {candidateId :: (B.C f Kernel.Prelude.Text),
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
                               updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table BackgroundVerificationT
    where data PrimaryKey BackgroundVerificationT f = BackgroundVerificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BackgroundVerificationId . driverId
type BackgroundVerification = BackgroundVerificationT Identity

$(enableKVPG (''BackgroundVerificationT) [('driverId)] [])

$(mkTableInstances (''BackgroundVerificationT) "background_verification")

