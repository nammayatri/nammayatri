{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AadhaarOtpVerify where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data AadhaarOtpVerifyT f = AadhaarOtpVerifyT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    requestId :: B.C f Kernel.Prelude.Text,
    requestMessage :: B.C f Kernel.Prelude.Text,
    statusCode :: B.C f Kernel.Prelude.Text,
    transactionId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AadhaarOtpVerifyT where
  data PrimaryKey AadhaarOtpVerifyT f = AadhaarOtpVerifyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AadhaarOtpVerifyId . id

type AadhaarOtpVerify = AadhaarOtpVerifyT Identity

$(enableKVPG ''AadhaarOtpVerifyT ['id] [['personId]])

$(mkTableInstances ''AadhaarOtpVerifyT "aadhaar_otp_verify")
