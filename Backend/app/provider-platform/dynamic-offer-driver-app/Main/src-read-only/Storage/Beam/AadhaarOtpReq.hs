{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AadhaarOtpReq where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data AadhaarOtpReqT f = AadhaarOtpReqT
  { driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    requestId :: (B.C f Kernel.Prelude.Text),
    requestMessage :: (B.C f Kernel.Prelude.Text),
    statusCode :: (B.C f Kernel.Prelude.Text),
    transactionId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AadhaarOtpReqT where
  data PrimaryKey AadhaarOtpReqT f = AadhaarOtpReqId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AadhaarOtpReqId . id

type AadhaarOtpReq = AadhaarOtpReqT Identity

$(enableKVPG (''AadhaarOtpReqT) [('id)] [[('driverId)]])

$(mkTableInstances (''AadhaarOtpReqT) "aadhaar_otp_req")
