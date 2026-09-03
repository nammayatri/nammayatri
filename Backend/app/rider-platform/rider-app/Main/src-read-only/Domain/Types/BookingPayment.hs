{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BookingPayment where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data BookingPayment = BookingPayment
  { bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    id :: Kernel.Types.Id.Id Domain.Types.BookingPayment.BookingPayment,
    paymentOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    paymentServiceType :: Lib.Payment.Domain.Types.PaymentOrder.PaymentServiceType,
    status :: Domain.Types.BookingPayment.BookingPaymentStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BookingPaymentStatus = PENDING | SUCCESS | FAILED | REFUND_PENDING | REFUNDED | REFUND_FAILED | REFUND_INITIATED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''BookingPaymentStatus))
