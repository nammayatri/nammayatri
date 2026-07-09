{-
  Finance.Invoice.Interface

  Input types for invoice operations.
  The actual operations are in Lib.Finance.Invoice.Service
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Invoice.Interface
  ( InvoiceInput (..),
    InvoiceLineItem (..),
    ItemType (..),
    LineItemDescription (..),
    GstAmountBreakdown (..),
    IndirectTaxInput (..),
    DirectTaxInput (..),
  )
where

import Domain.Types.Invoice (InvoiceType, IssuedToType)
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Lib.Finance.Domain.Types.DirectTaxTransaction (TdsRateReason)
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DirectTax
import Lib.Finance.Domain.Types.IndirectTaxTransaction (GstCreditType)
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax

-- | 'Fare'/'Tax' pair via shared 'groupId' and render together in the main table.
-- 'Adjustment' (Tip, Commission, discounts) renders below Total. External
-- charges (Toll, Parking) are flagged via 'isExternalCharge', not a separate type.
data ItemType = Tax | Fare | Adjustment
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Typed description key. The renderer maps each constructor to a locale-
-- specific display string. Keep this enum in sync with the creators that emit
-- new invoice line types.
data LineItemDescription
  = BaseFare
  | RideFare
  | RideFareInclVat
  | RideFarePostDiscount Currency HighPrecMoney
  | RideTax
  | TollFare
  | TollCharges
  | TollFareInclVat
  | TollTax
  | TollChargesTax
  | ParkingCharges
  | ParkingChargesInclVat
  | ParkingChargesTax
  | Tip
  | PlatformCommission
  | CancellationFee
  | CancellationFeeVat
  | CancellationFeeInclVat
  | CustomerCancellationFee
  | GstOnCancellationFee
  | DriverCancellationPenalty
  | SubscriptionPlanFee
  | Gst
  | WalletTopup
  | AirportCashRecharge
  | CashbackOffer
  | VatInput
  | RideFareRefund
  | RideFareRefundTax
  | TollRefund
  | TollRefundTax
  | ParkingRefund
  | ParkingRefundTax
  | CommissionRefund
  | PlatformCommissionTax
  | CommissionRefundTax
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | groupId is required for Fare/Tax (pairing); Nothing for Adjustment.
data InvoiceLineItem = InvoiceLineItem
  { description :: Text,
    descriptionType :: Maybe LineItemDescription,
    quantity :: Int,
    unitPrice :: HighPrecMoney,
    lineTotal :: HighPrecMoney,
    isExternalCharge :: Bool,
    groupId :: Maybe Text,
    itemType :: Maybe ItemType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Input for creating an invoice
data InvoiceInput = InvoiceInput
  { invoiceType :: InvoiceType,
    entityReferenceId :: Maybe Text, -- source entity ref: payment-order id (rides) / refund-request id (refunds)
    issuedToType :: IssuedToType,
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
    merchantGstin :: Maybe Text,
    referenceId :: Maybe Text,
    referenceInvoiceNumber :: Maybe Text, -- parent/original ride-invoice number (refund invoices)
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
    -- Period bounds for aggregated invoices; Nothing for per-event invoices.
    periodStart :: Maybe UTCTime,
    periodEnd :: Maybe UTCTime,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    merchantShortId :: Text,
    -- VAT integration fields
    isVat :: Bool,
    issuedToTaxNo :: Maybe Text,
    issuedByTaxNo :: Maybe Text,
    paymentMode :: Maybe Text
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
