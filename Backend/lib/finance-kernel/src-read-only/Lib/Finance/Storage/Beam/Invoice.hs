{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Finance.Storage.Beam.Invoice where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Data.Aeson
import qualified Database.Beam as B



data InvoiceT f
    = InvoiceT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                currency :: (B.C f Kernel.Types.Common.Currency),
                dueAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                id :: (B.C f Kernel.Prelude.Text),
                invoiceNumber :: (B.C f Kernel.Prelude.Text),
                invoiceType :: (B.C f Lib.Finance.Domain.Types.Invoice.InvoiceType),
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
                supplierAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                supplierGSTIN :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                supplierId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                supplierName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                supplierTaxNo :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                taxBreakdown :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                totalAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table InvoiceT
    where data PrimaryKey InvoiceT f = InvoiceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = InvoiceId . id
type Invoice = InvoiceT Identity

$(enableKVPG (''InvoiceT) [('id)] [[('invoiceNumber)], [('issuedToId)], [('paymentOrderId)], [('supplierId)]])

$(mkTableInstancesGenericSchema (''InvoiceT) "finance_invoice")

