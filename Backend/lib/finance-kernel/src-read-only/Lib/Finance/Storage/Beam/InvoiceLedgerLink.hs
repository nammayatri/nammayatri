{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.InvoiceLedgerLink where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude

data InvoiceLedgerLinkT f = InvoiceLedgerLinkT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    invoiceId :: (B.C f Kernel.Prelude.Text),
    ledgerEntryId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table InvoiceLedgerLinkT where
  data PrimaryKey InvoiceLedgerLinkT f = InvoiceLedgerLinkId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = InvoiceLedgerLinkId . id

type InvoiceLedgerLink = InvoiceLedgerLinkT Identity

$(enableKVPG (''InvoiceLedgerLinkT) [('id)] [[('invoiceId)], [('ledgerEntryId)]])

$(mkTableInstancesGenericSchema (''InvoiceLedgerLinkT) "finance_invoice_ledger_link")
