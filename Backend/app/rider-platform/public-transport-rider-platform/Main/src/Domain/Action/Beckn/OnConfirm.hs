{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnConfirm where

import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.PaymentTransaction as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import Kernel.Types.Id
import Kernel.Utils.Common
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

handleOnConfirm :: forall m r. EsqDBFlow m r => OnConfirmMessageD -> m ()
handleOnConfirm msg = do
  booking <- QBooking.findById msg.bookingId (Proxy @m) >>= fromMaybeM (BookingDoesNotExist msg.bookingId.getId)
  now <- getCurrentTime
  let updBooking =
        booking{status = msg.bookingStatus,
                ticketId = Just msg.ticketId,
                ticketCreatedAt = Just now
               }
  paymentData <- buildPaymentData updBooking msg
  runTransaction $ do
    QBooking.updateStatusAndBppOrderId @m updBooking
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
