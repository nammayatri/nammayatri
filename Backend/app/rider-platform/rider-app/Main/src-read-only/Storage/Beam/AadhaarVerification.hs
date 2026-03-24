{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.AadhaarVerification where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.External.Encryption
import qualified Database.Beam as B



data AadhaarVerificationT f
    = AadhaarVerificationT {aadhaarNumberHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
                            createdAt :: (B.C f Kernel.Prelude.UTCTime),
                            isVerified :: (B.C f Kernel.Prelude.Bool),
                            personDob :: (B.C f Kernel.Prelude.Text),
                            personGender :: (B.C f Kernel.Prelude.Text),
                            personId :: (B.C f Kernel.Prelude.Text),
                            personImagePath :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            personName :: (B.C f Kernel.Prelude.Text),
                            updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table AadhaarVerificationT
    where data PrimaryKey AadhaarVerificationT f = AadhaarVerificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = AadhaarVerificationId . personId
type AadhaarVerification = AadhaarVerificationT Identity

$(enableKVPG (''AadhaarVerificationT) [('personId)] [[('aadhaarNumberHash)]])

$(mkTableInstances (''AadhaarVerificationT) "aadhaar_verification")

