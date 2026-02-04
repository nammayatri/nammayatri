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

-- | Single line item in an invoice
data InvoiceLineItem = InvoiceLineItem
  { description :: Text,
    quantity :: Int,
    unitPrice :: HighPrecMoney,
    lineTotal :: HighPrecMoney
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Input for creating an invoice
data InvoiceInput = InvoiceInput
  { invoiceType :: Text,
    issuedToType :: Text,
    issuedToId :: Text,
    issuedToName :: Maybe Text,
    issuedByType :: Text,
    issuedById :: Text,
    issuedByName :: Maybe Text,
    lineItems :: [InvoiceLineItem],
    currency :: Currency,
    dueAt :: Maybe UTCTime,
    merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic)
