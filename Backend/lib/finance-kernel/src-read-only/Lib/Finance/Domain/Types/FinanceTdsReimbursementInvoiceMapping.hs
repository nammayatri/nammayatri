{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Tools.Beam.UtilsTH

data FinanceTdsReimbursementInvoiceMapping = FinanceTdsReimbursementInvoiceMapping
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping,
    invoiceId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice,
    requestId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest,
    revenueRecognisedSnapshot :: Kernel.Types.Common.HighPrecMoney,
    tdsAmount :: Kernel.Types.Common.HighPrecMoney,
    tdsCreditReceivable :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic)
