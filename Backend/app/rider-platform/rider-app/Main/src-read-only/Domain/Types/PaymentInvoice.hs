{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PaymentInvoice where

import Data.Aeson
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Ride
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data PaymentInvoice = PaymentInvoice
  { amount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Utils.Common.Currency,
    id :: Kernel.Types.Id.Id Domain.Types.PaymentInvoice.PaymentInvoice,
    invoiceNumber :: Kernel.Prelude.Text,
    invoiceType :: Domain.Types.PaymentInvoice.InvoiceType,
    paymentInstrument :: Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument,
    paymentOrderId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder),
    paymentPurpose :: Domain.Types.PaymentInvoice.PaymentPurpose,
    paymentStatus :: Domain.Types.PaymentInvoice.InvoicePaymentStatus,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data InvoicePaymentStatus = PENDING | CAPTURED | FAILED | CANCELLED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data InvoiceType = PAYMENT | REFUNDS deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data PaymentPurpose = RIDE | TIP | RIDE_TIP | CANCELLATION_FEE deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''InvoicePaymentStatus)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''PaymentPurpose)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''InvoiceType)
