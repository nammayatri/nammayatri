{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.Invoice where

import qualified Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id

data Invoice = Invoice
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    dueAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice,
    invoiceNumber :: Kernel.Prelude.Text,
    invoiceType :: Kernel.Prelude.Text,
    issuedAt :: Kernel.Prelude.UTCTime,
    issuedById :: Kernel.Prelude.Text,
    issuedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedByType :: Kernel.Prelude.Text,
    issuedToId :: Kernel.Prelude.Text,
    issuedToName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedToType :: Kernel.Prelude.Text,
    lineItems :: Data.Aeson.Value,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    paymentOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Lib.Finance.Domain.Types.Invoice.InvoiceStatus,
    subtotal :: Kernel.Types.Common.HighPrecMoney,
    taxBreakdown :: Kernel.Prelude.Maybe Data.Aeson.Value,
    totalAmount :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data InvoiceStatus = Draft | Issued | Paid | PartiallyPaid | Cancelled | Voided deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''InvoiceStatus))
