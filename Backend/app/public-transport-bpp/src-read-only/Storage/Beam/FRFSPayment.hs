{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSPayment where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSPayment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSPaymentT f = FRFSPaymentT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    frfsTicketBookingId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    paymentReferenceNumber :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.FRFSPayment.PaymentStatusEnum),
    transactionId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSPaymentT where
  data PrimaryKey FRFSPaymentT f = FRFSPaymentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSPaymentId . id

type FRFSPayment = FRFSPaymentT Identity

$(enableKVPG (''FRFSPaymentT) [('id)] [])

$(mkTableInstances (''FRFSPaymentT) "frfs_payment")
