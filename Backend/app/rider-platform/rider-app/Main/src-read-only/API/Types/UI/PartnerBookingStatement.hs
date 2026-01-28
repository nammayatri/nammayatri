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
    paymentAmount :: Kernel.Types.Common.HighPrecMoney,
    paymentAmountCurrencyCode :: Kernel.Prelude.Text,
    provider :: Kernel.Prelude.Text,
    providerCode :: Kernel.Prelude.Text,
    serviceEndAddress :: Kernel.Prelude.Text,
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
  { emailId :: Kernel.Prelude.Text,
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
  { addonAmount :: Kernel.Types.Common.HighPrecMoney,
    couponAmount :: Kernel.Types.Common.HighPrecMoney,
    couponCode :: Kernel.Prelude.Text,
    discountAmount :: Kernel.Types.Common.HighPrecMoney,
    itemAmount :: Kernel.Types.Common.HighPrecMoney,
    paidByPoint :: Kernel.Types.Common.HighPrecMoney,
    paymentAmount :: Kernel.Types.Common.HighPrecMoney,
    paymentAmountCurrencyName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    taxableAmount :: Kernel.Types.Common.HighPrecMoney
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
    emailId :: Kernel.Prelude.Text,
    employerGst :: Kernel.Prelude.Text,
    employerName :: Kernel.Prelude.Text,
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
  { invoiceAmount :: Kernel.Types.Common.HighPrecMoney,
    invoiceAmountCurrencyName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    invoiceDatetime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    invoiceLink :: Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoiceItem = InvoiceItem
  { bookingClass :: Domain.Types.ServiceTierType.ServiceTierType,
    bookingQuota :: Kernel.Prelude.Text,
    bookingType :: Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory,
    itemId :: Kernel.Prelude.Text,
    itemName :: Kernel.Prelude.Text,
    itemQuantity :: Kernel.Prelude.Text,
    personCount :: Kernel.Prelude.Text,
    provider :: Kernel.Prelude.Text,
    providerCode :: Kernel.Prelude.Text,
    providerPriceCategory :: Kernel.Prelude.Text,
    serviceEndAddress :: Kernel.Prelude.Text,
    serviceEndDatetime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    serviceEndPincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceStartAddress :: Kernel.Prelude.Text,
    serviceStartDatetime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    serviceStartPincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoicePaymentMode = InvoicePaymentMode
  { amount :: Kernel.Types.Common.HighPrecMoney,
    cardEnd :: Kernel.Prelude.Text,
    cardStart :: Kernel.Prelude.Text,
    issuerName :: Kernel.Prelude.Text,
    paymentMode :: Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoicePerson = InvoicePerson
  { dob :: Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Text,
    gender :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Text,
    prefix :: Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data InvoiceUnitPricing = InvoiceUnitPricing
  { discountAmount :: Kernel.Types.Common.HighPrecMoney,
    itemAmount :: Kernel.Types.Common.HighPrecMoney,
    itemId :: Kernel.Prelude.Text,
    loyaltyNumber :: Kernel.Prelude.Text,
    loyaltyProvider :: Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Text,
    platformFee :: Kernel.Types.Common.HighPrecMoney,
    priceMultipler :: Kernel.Prelude.Double,
    shippingFee :: Kernel.Types.Common.HighPrecMoney,
    taxAmount :: Kernel.Types.Common.HighPrecMoney,
    taxableAmount :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)
