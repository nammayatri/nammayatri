{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AadhaarVerification where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data AadhaarVerificationT f = AadhaarVerificationT
  { aadhaarNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    isVerified :: B.C f Kernel.Prelude.Bool,
    personDob :: B.C f Kernel.Prelude.Text,
    personGender :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    personImagePath :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    personName :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AadhaarVerificationT where
  data PrimaryKey AadhaarVerificationT f = AadhaarVerificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AadhaarVerificationId . personId

type AadhaarVerification = AadhaarVerificationT Identity

$(enableKVPG ''AadhaarVerificationT ['personId] [['aadhaarNumberHash]])

$(mkTableInstances ''AadhaarVerificationT "aadhaar_verification")
