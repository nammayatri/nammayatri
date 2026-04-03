{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FRFSTicketBookingPayment where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSTicketBooking
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data FRFSTicketBookingPayment
    = FRFSTicketBookingPayment {frfsQuoteId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote),
                                frfsTicketBookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
                                id :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment,
                                paymentOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
                                status :: Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus,
                                merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                                merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                                createdAt :: Kernel.Prelude.UTCTime,
                                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data FRFSTicketBookingPaymentStatus = PENDING | SUCCESS | FAILED | REFUND_PENDING | REFUNDED | REFUND_FAILED | REFUND_INITIATED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FRFSTicketBookingPaymentStatus))

