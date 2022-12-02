module Domain.Action.Beckn.OnConfirm where

import Beckn.Prelude
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.PaymentTransaction as Domain
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as PaymentTransactionDB
import Tools.Error

data OnConfirmMessageD = OnConfirmMessageD
  { bookingId :: Id Domain.Booking,
    ticketId :: Text,
    paymentGatewayTxnId :: Text,
    paymentGatewayTxnStatus :: Text,
    bookingStatus :: Domain.BookingStatus,
    paymentStatus :: Domain.PaymentStatus,
    paymentUrl :: BaseUrl
  }

handleOnConfirm :: EsqDBFlow m r => OnConfirmMessageD -> m ()
handleOnConfirm msg = do
  booking <- QBooking.findById msg.bookingId >>= fromMaybeM (BookingDoesNotExist msg.bookingId.getId)
  now <- getCurrentTime
  let updBooking =
        booking{status = msg.bookingStatus,
                ticketId = Just msg.ticketId,
                ticketCreatedAt = Just now
               }
  paymentData <- buildPaymentData updBooking msg
  runTransaction $ do
    QBooking.updateStatusAndBppOrderId updBooking
    PaymentTransactionDB.create paymentData

buildPaymentData :: MonadFlow m => Domain.Booking -> OnConfirmMessageD -> m Domain.PaymentTransaction
buildPaymentData booking msg = do
  id <- generateGUID
  now <- getCurrentTime
  return
    Domain.PaymentTransaction
      { id = Id id,
        bookingId = booking.id,
        bknTxnId = booking.bknTxnId,
        paymentGatewayTxnId = msg.paymentGatewayTxnId,
        paymentGatewayTxnStatus = msg.paymentGatewayTxnStatus,
        fare = booking.fare,
        status = msg.paymentStatus,
        paymentUrl = msg.paymentUrl,
        updatedAt = now,
        createdAt = now
      }
