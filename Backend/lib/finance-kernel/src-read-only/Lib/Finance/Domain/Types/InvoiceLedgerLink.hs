{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.InvoiceLedgerLink where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Domain.Types.LedgerEntry
import qualified Tools.Beam.UtilsTH

data InvoiceLedgerLink = InvoiceLedgerLink
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink,
    invoiceId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice,
    ledgerEntryId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
