{-
  Finance.Invoice.Interface

  Input types for invoice operations.
  The actual operations are in Lib.Finance.Invoice.Service
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Invoice.Interface
  ( InvoiceInput (..),
    InvoiceLineItem (..),
  )
where

import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
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
    supplierId :: Maybe Text,
    gstinOfParty :: Maybe Text,
    lineItems :: [InvoiceLineItem],
    currency :: Currency,
    dueAt :: Maybe UTCTime,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    merchantShortId :: Text
  }
  deriving (Eq, Show, Generic)
