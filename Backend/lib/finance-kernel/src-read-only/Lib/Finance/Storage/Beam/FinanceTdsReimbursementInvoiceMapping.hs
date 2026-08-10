{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.FinanceTdsReimbursementInvoiceMapping where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FinanceTdsReimbursementInvoiceMappingT f = FinanceTdsReimbursementInvoiceMappingT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    invoiceId :: (B.C f Kernel.Prelude.Text),
    requestId :: (B.C f Kernel.Prelude.Text),
    revenueRecognisedSnapshot :: (B.C f Kernel.Types.Common.HighPrecMoney),
    tdsAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    tdsCreditReceivable :: (B.C f Kernel.Types.Common.HighPrecMoney)
  }
  deriving (Generic, B.Beamable)

instance B.Table FinanceTdsReimbursementInvoiceMappingT where
  data PrimaryKey FinanceTdsReimbursementInvoiceMappingT f = FinanceTdsReimbursementInvoiceMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FinanceTdsReimbursementInvoiceMappingId . id

type FinanceTdsReimbursementInvoiceMapping = FinanceTdsReimbursementInvoiceMappingT Identity

$(enableKVPG (''FinanceTdsReimbursementInvoiceMappingT) [('id)] [[('invoiceId)], [('requestId)]])

$(mkTableInstancesGenericSchema (''FinanceTdsReimbursementInvoiceMappingT) "finance_tds_reimbursement_invoice_mapping")
