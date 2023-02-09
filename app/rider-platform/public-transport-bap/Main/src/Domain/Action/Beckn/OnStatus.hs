module Domain.Action.Beckn.OnStatus where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.PaymentTransaction as DPaymentTransaction
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (PrettyShow)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as QPaymentTransaction
import Tools.Error

data OnStatusReq = OnStatusReq
  { bookingId :: Id DBooking.Booking,
    bookingStatus :: DBooking.BookingStatus,
    transactionStatus :: Text,
    paymentStatus :: DPaymentTransaction.PaymentStatus
  }
  deriving (Show, Generic, PrettyShow)

handler :: EsqDBFlow m r => OnStatusReq -> m ()
handler OnStatusReq {..} = do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  paymentDetails <- QPaymentTransaction.findByBookingId booking.id >>= fromMaybeM (PaymentDetailsNotFound booking.id.getId)
  runTransaction $ do
    QBooking.updateStatus booking bookingStatus
    QPaymentTransaction.updateTxnDetails paymentDetails.id transactionStatus paymentStatus
