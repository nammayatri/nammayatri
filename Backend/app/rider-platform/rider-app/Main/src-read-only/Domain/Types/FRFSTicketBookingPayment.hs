{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.FRFSTicketBookingPayment where

import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Tools.Beam.UtilsTH

data FRFSTicketBookingPayment = FRFSTicketBookingPayment
  { frfsTicketBookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment,
    paymentOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    status :: Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FRFSTicketBookingPaymentStatus = PENDING | SUCCESS | FAILED | REFUND_PENDING | REFUNDED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''FRFSTicketBookingPaymentStatus)
