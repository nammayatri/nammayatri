{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.AadhaarOtpReq where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data AadhaarOtpReqT f
    = AadhaarOtpReqT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      id :: (B.C f Kernel.Prelude.Text),
                      personId :: (B.C f Kernel.Prelude.Text),
                      requestId :: (B.C f Kernel.Prelude.Text),
                      requestMessage :: (B.C f Kernel.Prelude.Text),
                      statusCode :: (B.C f Kernel.Prelude.Text),
                      transactionId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      updatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime))}
    deriving (Generic, B.Beamable)
instance B.Table AadhaarOtpReqT
    where data PrimaryKey AadhaarOtpReqT f = AadhaarOtpReqId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = AadhaarOtpReqId . id
type AadhaarOtpReq = AadhaarOtpReqT Identity

$(enableKVPG (''AadhaarOtpReqT) [('id)] [[('personId)]])

$(mkTableInstances (''AadhaarOtpReqT) "aadhaar_otp_req")

