{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BookingPayment where

import qualified Database.Beam as B
import qualified Domain.Types.BookingPayment
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Tools.Beam.UtilsTH

data BookingPaymentT f = BookingPaymentT
  { bookingId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    paymentOrderId :: (B.C f Kernel.Prelude.Text),
    paymentServiceType :: (B.C f Lib.Payment.Domain.Types.PaymentOrder.PaymentServiceType),
    status :: (B.C f Domain.Types.BookingPayment.BookingPaymentStatus),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingPaymentT where
  data PrimaryKey BookingPaymentT f = BookingPaymentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BookingPaymentId . id

type BookingPayment = BookingPaymentT Identity

$(enableKVPG (''BookingPaymentT) [('id)] [[('bookingId)], [('paymentOrderId)]])

$(mkTableInstances (''BookingPaymentT) "booking_payment")
