{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AadhaarVerification where

import qualified Database.Beam as B
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data AadhaarVerificationT f = AadhaarVerificationT
  { aadhaarNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    driverDob :: B.C f Kernel.Prelude.Text,
    driverGender :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    driverImage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverImagePath :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverName :: B.C f Kernel.Prelude.Text,
    isVerified :: B.C f Kernel.Prelude.Bool,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AadhaarVerificationT where
  data PrimaryKey AadhaarVerificationT f = AadhaarVerificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AadhaarVerificationId . driverId

type AadhaarVerification = AadhaarVerificationT Identity

$(enableKVPG ''AadhaarVerificationT ['driverId] [['aadhaarNumberHash]])

$(mkTableInstances ''AadhaarVerificationT "aadhaar_verification")
