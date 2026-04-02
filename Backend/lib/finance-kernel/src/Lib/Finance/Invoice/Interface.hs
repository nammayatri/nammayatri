{-
  Finance.Invoice.Interface

  Input types for invoice operations.
  The actual operations are in Lib.Finance.Invoice.Service
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Invoice.Interface
  ( InvoiceInput (..),
    InvoiceLineItem (..),
    GstAmountBreakdown (..),
    IndirectTaxInput (..),
    DirectTaxInput (..),
  )
where

import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Lib.Finance.Domain.Types.DirectTaxTransaction (TdsRateReason)
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DirectTax
import Lib.Finance.Domain.Types.IndirectTaxTransaction (GstCreditType)
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import Lib.Finance.Domain.Types.Invoice (InvoiceType)

-- | Single line item in an invoice
data InvoiceLineItem = InvoiceLineItem
  { description :: Text,
    quantity :: Int,
    unitPrice :: HighPrecMoney,
    lineTotal :: HighPrecMoney,
    -- | True for pass-through charges (toll, parking) excluded from taxable value
    isExternalCharge :: Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Input for creating an invoice
data InvoiceInput = InvoiceInput
  { invoiceType :: InvoiceType,
    paymentOrderId :: Maybe Text,
    issuedToType :: Text,
    issuedToId :: Text,
    issuedToName :: Maybe Text,
    issuedToAddress :: Maybe Text,
    issuedByType :: Text,
    issuedById :: Text,
    issuedByName :: Maybe Text,
    issuedByAddress :: Maybe Text,
    supplierName :: Maybe Text,
    supplierAddress :: Maybe Text,
    supplierGSTIN :: Maybe Text,
    supplierTaxNo :: Maybe Text,
    supplierId :: Maybe Text,
    gstinOfParty :: Maybe Text,
    panOfParty :: Maybe Text,
    panType :: Maybe Text,
    counterpartyId :: Text, -- driver/fleet-owner ID for TDS counterparty
    tdsRateReason :: Maybe TdsRateReason,
    tanOfDeductee :: Maybe Text,
    lineItems :: [InvoiceLineItem],
    gstBreakdown :: Maybe GstAmountBreakdown, -- caller-provided CGST/SGST/IGST amounts
    currency :: Currency,
    dueAt :: Maybe UTCTime,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    merchantShortId :: Text,
    -- VAT integration fields
    isVat :: Bool,
    issuedToTaxNo :: Maybe Text,
    issuedByTaxNo :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Pre-computed GST amount breakdown from the caller.
--   Avoids re-deriving the split inside the invoice service.
data GstAmountBreakdown = GstAmountBreakdown
  { cgstAmount :: Maybe HighPrecMoney,
    sgstAmount :: Maybe HighPrecMoney,
    igstAmount :: Maybe HighPrecMoney
  }
  deriving (Eq, Show, Generic)

-- | Input for creating a standalone indirect tax (GST/VAT) transaction
--   without an invoice.
data IndirectTaxInput = IndirectTaxInput
  { transactionType :: IndirectTax.TransactionType,
    referenceId :: Text,
    taxableValue :: HighPrecMoney,
    totalTaxAmount :: HighPrecMoney, -- renamed from totalGstAmount: the actual tax amount (GST or VAT)
    gstBreakdown :: Maybe GstAmountBreakdown,
    taxCreditType :: GstCreditType,
    counterpartyId :: Text,
    gstinOfParty :: Maybe Text,
    sacCode :: Maybe Text,
    externalCharges :: Maybe HighPrecMoney,
    invoiceNumber :: Maybe Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    -- VAT integration fields
    isVat :: Bool,
    issuedToTaxNo :: Maybe Text,
    issuedByTaxNo :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Input for creating a standalone direct tax (TDS) transaction
--   without an invoice.
data DirectTaxInput = DirectTaxInput
  { transactionType :: DirectTax.TransactionType,
    referenceId :: Text,
    grossAmount :: HighPrecMoney,
    tdsAmount :: HighPrecMoney,
    tdsTreatment :: DirectTax.TdsTreatment,
    counterpartyId :: Text,
    panOfParty :: Maybe Text,
    panType :: Maybe Text,
    tdsRateReason :: Maybe TdsRateReason,
    tanOfDeductee :: Maybe Text,
    tdsSection :: Maybe Text,
    invoiceNumber :: Maybe Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic)
