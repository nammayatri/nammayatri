{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

handler :: forall m r. EsqDBFlow m r => OnStatusReq -> m ()
handler OnStatusReq {..} = do
  booking <- QBooking.findById bookingId (Proxy @m) >>= fromMaybeM (BookingNotFound bookingId.getId)
  paymentDetails <- QPaymentTransaction.findByBookingId booking.id (Proxy @m) >>= fromMaybeM (PaymentDetailsNotFound booking.id.getId)
  runTransaction $ do
    QBooking.updateStatus @m booking bookingStatus
    QPaymentTransaction.updateTxnDetails paymentDetails.id transactionStatus paymentStatus
