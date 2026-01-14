{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PaymentInvoice where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.PaymentInvoice
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Utils.Common
import Tools.Beam.UtilsTH

data PaymentInvoiceT f = PaymentInvoiceT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f Kernel.Utils.Common.Currency,
    id :: B.C f Kernel.Prelude.Text,
    invoiceNumber :: B.C f Kernel.Prelude.Text,
    invoiceType :: B.C f Domain.Types.PaymentInvoice.InvoiceType,
    paymentInstrument :: B.C f Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument,
    paymentOrderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentPurpose :: B.C f Domain.Types.PaymentInvoice.PaymentPurpose,
    paymentStatus :: B.C f Domain.Types.PaymentInvoice.InvoicePaymentStatus,
    rideId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PaymentInvoiceT where
  data PrimaryKey PaymentInvoiceT f = PaymentInvoiceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PaymentInvoiceId . id

type PaymentInvoice = PaymentInvoiceT Identity

$(enableKVPG ''PaymentInvoiceT ['id] [['invoiceNumber], ['paymentOrderId], ['rideId]])

$(mkTableInstances ''PaymentInvoiceT "payment_invoice")
