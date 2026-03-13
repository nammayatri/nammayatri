{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CorporateInvoice where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CorporateInvoiceT f = CorporateInvoiceT
  { id :: B.C f Kernel.Prelude.Text,
    corporateEntityId :: B.C f Kernel.Prelude.Text,
    invoiceNumber :: B.C f Kernel.Prelude.Text,
    periodStart :: B.C f Kernel.Prelude.UTCTime,
    periodEnd :: B.C f Kernel.Prelude.UTCTime,
    totalTrips :: B.C f Kernel.Prelude.Int,
    baseAmount :: B.C f Kernel.Prelude.Double,
    cgstRate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    cgstAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    sgstRate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    sgstAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    igstRate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    igstAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    totalTaxAmount :: B.C f Kernel.Prelude.Double,
    netAmount :: B.C f Kernel.Prelude.Double,
    currency :: B.C f Kernel.Prelude.Text,
    sacCode :: B.C f Kernel.Prelude.Text,
    placeOfSupply :: B.C f Kernel.Prelude.Text,
    supplierGstin :: B.C f Kernel.Prelude.Text,
    recipientGstin :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    eInvoiceIrn :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Kernel.Prelude.Text,
    pdfUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    generatedAt :: B.C f Kernel.Prelude.UTCTime,
    paidAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CorporateInvoiceT where
  data PrimaryKey CorporateInvoiceT f = CorporateInvoiceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CorporateInvoiceId . id

type CorporateInvoice = CorporateInvoiceT Identity

$(enableKVPG ''CorporateInvoiceT ['id] [['corporateEntityId], ['invoiceNumber]])

$(mkTableInstances ''CorporateInvoiceT "corporate_invoice")
