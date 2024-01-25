{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Action.Beckn.FRFS.OnStatus where

import qualified Beckn.ACL.FRFS.Utils as Utils
import Domain.Action.Beckn.FRFS.Common
import qualified Domain.Types.FRFSTicketBooking as Booking
import Domain.Types.Merchant as Merchant
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QTBooking

validateRequest :: DOrder -> Flow (Merchant, Booking.FRFSTicketBooking)
validateRequest DOrder {..} = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  booking <- runInReplica $ QTBooking.findByBppOrderId (Just bppOrderId) >>= fromMaybeM (BookingDoesNotExist messageId)
  merchantId <- booking.merchantId & fromMaybeM (InternalError "MerchantId not found in booking") -- TODO: Make merchantId required
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, booking)

onStatus :: Merchant -> Booking.FRFSTicketBooking -> DOrder -> Flow ()
onStatus _merchant booking dOrder = do
  statuses <- traverse Utils.getTicketStatus dOrder.tickets
  void $ traverse updateTicket statuses
  where
    updateTicket (ticketNumber, status) =
      void $ QTicket.updateStatusByTBookingIdAndTicketNumber status booking.id ticketNumber
