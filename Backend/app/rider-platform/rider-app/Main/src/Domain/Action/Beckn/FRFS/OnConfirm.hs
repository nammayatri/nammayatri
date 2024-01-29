{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnConfirm where

import qualified Beckn.ACL.FRFS.Cancel as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import Domain.Action.Beckn.FRFS.Common
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicket as Ticket
import qualified Domain.Types.FRFSTicketBooking as Booking
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Domain.Types.Merchant as Merchant
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallBPP
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QTBooking

validateRequest :: DOrder -> Flow (Merchant, Booking.FRFSTicketBooking)
validateRequest DOrder {..} = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  booking <- runInReplica $ QTBooking.findById (Id messageId) >>= fromMaybeM (BookingDoesNotExist messageId)
  merchantId <- booking.merchantId & fromMaybeM (InternalError "MerchantId not found in booking")
  now <- getCurrentTime

  if booking.validTill < now
    then do
      -- Booking is expired
      bapConfig <- QBC.findByMerchantIdAndDomain (Just merchantId) (show Spec.FRFS) >>= fromMaybeM (InternalError "Beckn Config not found")
      void $ QTBooking.updateBPPOrderIdAndStatusById (Just bppOrderId) Booking.FAILED booking.id
      callBPPCancel booking bapConfig
      throwM $ (InvalidRequest "Booking expired, initated cancel request")
    else do
      -- Booking is valid, proceed to onConfirm handler
      merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      return (merchant, booking)

onConfirm :: Merchant -> Booking.FRFSTicketBooking -> DOrder -> Flow ()
onConfirm _merchant booking dOrder = do
  tickets <- traverse (mkTicket booking) dOrder.tickets
  void $ QTicket.createMany tickets
  void $ QTBooking.updateBPPOrderIdAndStatusById (Just dOrder.bppOrderId) Booking.CONFIRMED booking.id
  return ()

mkTicket :: Booking.FRFSTicketBooking -> DTicket -> Flow Ticket.FRFSTicket
mkTicket booking dTicket = do
  now <- getCurrentTime
  ticketId <- generateGUID
  (_, status) <- Utils.getTicketStatus dTicket

  return
    Ticket.FRFSTicket
      { Ticket.frfsTicketBookingId = booking.id,
        Ticket.id = ticketId,
        Ticket.qrData = dTicket.qrData,
        Ticket.riderId = booking.riderId,
        Ticket.status = status,
        Ticket.ticketNumber = dTicket.ticketNumber,
        Ticket.validTill = dTicket.validTill,
        Ticket.merchantId = booking.merchantId,
        Ticket.merchantOperatingCityId = booking.merchantOperatingCityId,
        Ticket.createdAt = now,
        Ticket.updatedAt = now
      }

callBPPCancel :: DFRFSTicketBooking.FRFSTicketBooking -> BecknConfig -> Environment.Flow ()
callBPPCancel booking bapConfig = do
  fork "FRFS Cancel Req" $ do
    providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
    bknStatusReq <- ACL.buildCancelReq booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl}
    void $ CallBPP.cancel providerUrl bknStatusReq
