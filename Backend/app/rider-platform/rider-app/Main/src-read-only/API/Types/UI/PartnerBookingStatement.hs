{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PartnerBookingStatement where

import qualified BecknV2.OnDemand.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Time.Calendar
import qualified Domain.Types.Booking
import qualified Domain.Types.BookingStatus
import qualified Domain.Types.ServiceTierType
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data BookingStatementItem = BookingStatementItem
  { bookingDatetime :: Kernel.Prelude.UTCTime,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    bookingType :: Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory,
    paymentAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    paymentAmountCurrencyCode :: Kernel.Prelude.Text,
    provider :: Kernel.Prelude.Text,
    providerCode :: Kernel.Prelude.Text,
    serviceEndAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceEndDatetime :: Kernel.Prelude.UTCTime,
    serviceEndPincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceStartAddress :: Kernel.Prelude.Text,
    serviceStartDatetime :: Kernel.Prelude.UTCTime,
    serviceStartPincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data BookingStatementReq = BookingStatementReq
  { count :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    emailId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromDate :: Data.Time.Calendar.Day,
    mobileNumber :: Kernel.Prelude.Text,
    page :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    partnerCustomerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toDate :: Data.Time.Calendar.Day
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data BookingStatementRes = BookingStatementRes
  { emailId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    item :: [BookingStatementItem],
    mobileNumber :: Kernel.Prelude.Text,
    outcomeCode :: Kernel.Prelude.Text,
    partnerCustomerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseCode :: Kernel.Prelude.Text,
    responseMessage :: Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoiceBilling = InvoiceBilling
  { addonAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    discountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    itemAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    paymentAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    paymentAmountCurrencyName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoiceDataReq = InvoiceDataReq
  { bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    emailId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    partnerCustomerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoiceDataRes = InvoiceDataRes
  { billing :: InvoiceBilling,
    bookingDatetime :: Kernel.Prelude.UTCTime,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    bookingStatus :: Domain.Types.BookingStatus.BookingStatus,
    emailId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gst :: [InvoiceGST],
    invoice :: InvoiceInvoice,
    item :: [InvoiceItem],
    mobileNumber :: Kernel.Prelude.Text,
    outcomeCode :: Kernel.Prelude.Text,
    partnerCustomerId :: Kernel.Prelude.Text,
    paymentMode :: [InvoicePaymentMode],
    person :: [InvoicePerson],
    responseCode :: Kernel.Prelude.Text,
    responseMessage :: Kernel.Prelude.Text,
    unitPricing :: [InvoiceUnitPricing]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoiceGST = InvoiceGST
  { cgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    igst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    merchantGst :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sac :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoiceInvoice = InvoiceInvoice
  { invoiceAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    invoiceAmountCurrencyName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    invoiceDatetime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoiceItem = InvoiceItem
  { bookingClass :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    bookingType :: Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory,
    itemId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    itemName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    provider :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerPriceCategory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceEndAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceEndDatetime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    serviceEndPincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceStartAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceStartDatetime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    serviceStartPincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoicePaymentMode = InvoicePaymentMode {amount :: Kernel.Prelude.Maybe Kernel.Types.Common.Money, paymentMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoicePerson = InvoicePerson
  { firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gender :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoiceUnitPricing = InvoiceUnitPricing
  { discountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    itemAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    itemId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Text,
    platformFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    priceMultipler :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)
