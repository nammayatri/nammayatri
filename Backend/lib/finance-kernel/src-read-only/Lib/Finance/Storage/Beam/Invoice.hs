{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.Invoice where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Invoice

data InvoiceT f = InvoiceT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    dueAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    id :: (B.C f Kernel.Prelude.Text),
    invoiceNumber :: (B.C f Kernel.Prelude.Text),
    invoiceType :: (B.C f Kernel.Prelude.Text),
    issuedAt :: (B.C f Kernel.Prelude.UTCTime),
    issuedByAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    issuedById :: (B.C f Kernel.Prelude.Text),
    issuedByName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    issuedByType :: (B.C f Kernel.Prelude.Text),
    issuedToAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    issuedToId :: (B.C f Kernel.Prelude.Text),
    issuedToName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    issuedToType :: (B.C f Kernel.Prelude.Text),
    lineItems :: (B.C f Data.Aeson.Value),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    paymentOrderId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    status :: (B.C f Lib.Finance.Domain.Types.Invoice.InvoiceStatus),
    subtotal :: (B.C f Kernel.Types.Common.HighPrecMoney),
    taxBreakdown :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    totalAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table InvoiceT where
  data PrimaryKey InvoiceT f = InvoiceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = InvoiceId . id

type Invoice = InvoiceT Identity

$(enableKVPG (''InvoiceT) [('id)] [[('invoiceNumber)], [('issuedToId)], [('paymentOrderId)]])

$(mkTableInstancesGenericSchema (''InvoiceT) "finance_invoice")
