{-# LANGUAGE DeriveAnyClass #-}

module Domain.Types.Extra.PaymentInvoice where


import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Domain.Types.PaymentInvoice (InvoicePaymentStatus, InvoiceType, PaymentInvoice, PaymentPurpose)
import EulerHS.Prelude
import Kernel.Types.Common (Currency, Money)
import Kernel.Types.Id (Id)

data InvoiceAPIEntity = InvoiceAPIEntity
  { invoiceId :: Id PaymentInvoice,
    invoiceNumber :: Text,
    amount :: Money,
    currency :: Currency,
    invoiceType :: InvoiceType,
    paymentPurpose :: PaymentPurpose,
    paymentStatus :: InvoicePaymentStatus,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
