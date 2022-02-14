module Domain.Endpoints.Beckn.OnStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding ((<&>))
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.PaymentTransaction as DPaymentTransaction
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as QPaymentTransaction
import Tools.Error

data OnStatusReq = OnStatusReq
  { bookingId :: Id DBooking.Booking,
    bookingStatus :: DBooking.BookingStatus,
    transactionStatus :: Text,
    paymentStatus :: DPaymentTransaction.PaymentStatus
  }

handler :: EsqDBFlow m r => OnStatusReq -> m ()
handler OnStatusReq {..} = do
  booking <- QBooking.findById bookingId >>= fromMaybeM BookingNotFound
  paymentDetails <- QPaymentTransaction.findByBookingId booking.id >>= fromMaybeM PaymentDetailsNotFound
  runTransaction $ do
    QBooking.updateStatus booking bookingStatus
    QPaymentTransaction.updateTxnDetails paymentDetails.id transactionStatus paymentStatus
