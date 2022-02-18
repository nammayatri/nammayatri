module Domain.Endpoints.Beckn.OnConfirm where

import Beckn.Prelude
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Id
import Beckn.Utils.Common
import Core.Spec.Common.Payment (TrStatus)
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.PaymentTransaction as Domain
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as PaymentTransactionDB
import Tools.Error

data OnConfirmMessageD = OnConfirmMessageD
  { bookingId :: Id Domain.Booking,
    ticketId :: Text,
    paymentGatewayTxnId :: Text,
    paymentGatewayTxnStatus :: TrStatus,
    paymentUrl :: BaseUrl
  }

handleOnConfirm :: EsqDBFlow m r => OnConfirmMessageD -> m ()
handleOnConfirm msg = do
  booking <- QBooking.findById msg.bookingId >>= fromMaybeM BookingDoesNotExist
  now <- getCurrentTime
  let updBooking =
        booking{status = Domain.AWAITING_PAYMENT,
                --                bppOrderId = Just msg.order.id,
                --                that was present in parking, should we use it here?
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
        paymentGatewayTxnStatus = show msg.paymentGatewayTxnStatus,
        fare = booking.fare,
        status = Domain.PENDING,
        paymentUrl = msg.paymentUrl,
        updatedAt = now,
        createdAt = now
      }
