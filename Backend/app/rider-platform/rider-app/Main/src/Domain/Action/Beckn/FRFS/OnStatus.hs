{-
statuses Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Action.Beckn.FRFS.OnStatus where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import Data.Tuple.Extra
import Domain.Action.Beckn.FRFS.Common
import qualified Domain.Action.Beckn.FRFS.GWLink as GWSA
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.FRFSTicket as Ticket
import qualified Domain.Types.FRFSTicketBooking as Booking
import Domain.Types.Merchant as Merchant
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude hiding (second)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Utils.Common.JWT.Config as GW
import qualified Utils.Common.JWT.TransitClaim as TC

data DOnStatus = Booking DOrder | TicketVerification DTicketPayload

validateRequest :: DOnStatus -> Flow (Merchant, Booking.FRFSTicketBooking)
validateRequest (Booking DOrder {..}) = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  booking <- runInReplica $ QTBooking.findByBppOrderId (Just bppOrderId) >>= fromMaybeM (BookingDoesNotExist messageId)
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, booking)
validateRequest (TicketVerification DTicketPayload {..}) = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  booking <- runInReplica $ QTBooking.findBySearchId (Id transactionId) >>= fromMaybeM (BookingDoesNotExist transactionId)
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, booking)

onStatus :: Merchant -> Booking.FRFSTicketBooking -> DOnStatus -> Flow ()
onStatus _merchant booking (Booking dOrder) = do
  statuses <- traverse (Utils.getTicketStatus booking) dOrder.tickets
  let googleWalletStates = map (second GWSA.mapToGoogleTicketStatus) statuses
  whenJust dOrder.orderStatus $ \status ->
    case status of
      Spec.COMPLETE | booking.status == Booking.CANCEL_INITIATED -> QTBooking.updateStatusById Booking.TECHNICAL_CANCEL_REJECTED booking.id
      Spec.CANCELLED | not booking.customerCancelled -> QTBooking.updateStatusById Booking.COUNTER_CANCELLED booking.id
      _ -> pure ()
  traverse_ updateTicket statuses
  fork "updating status for google wallet " $ traverse_ updateStatesForGoogleWallet googleWalletStates
  traverse_ refreshTicket dOrder.tickets
  where
    updateTicket (ticketNumber, status) =
      void $ QTicket.updateStatusByTBookingIdAndTicketNumber status booking.id ticketNumber
    updateStatesForGoogleWallet (ticketNumber, state') = do
      let serviceName = DEMSC.WalletService GW.GoogleWallet
      let mId = booking.merchantId
      let mocId' = booking.merchantOperatingCityId
      serviceAccount <- GWSA.getserviceAccount mId mocId' serviceName
      ticket <- runInReplica $ QTicket.findByTicketBookingIdTicketNumber booking.id ticketNumber >>= fromMaybeM (InternalError "Ticket Does Not Exist")
      let resourceId = serviceAccount.saIssuerId <> "." <> ticket.id.getId
      let obj = TC.TransitObjectPatch {TC.state = show state'}
      void $ GWSA.updateTicketStatusForGoogleWallet obj serviceAccount resourceId
    refreshTicket ticket =
      whenJust ticket.qrRefreshAt $ \qrRefreshAt ->
        void $ QTicket.updateRefreshTicketQRByTBookingIdAndTicketNumber ticket.qrData (Just qrRefreshAt) booking.id ticket.ticketNumber
onStatus _merchant booking (TicketVerification ticketPayload) = do
  ticket <- runInReplica $ QTicket.findByTicketBookingIdTicketNumber booking.id ticketPayload.ticketNumber >>= fromMaybeM (InternalError "Ticket Does Not Exist")
  unless (ticket.status == Ticket.ACTIVE) $ throwError (InvalidRequest "Ticket is not in Active state")
  void $ QTicket.updateStatusByTBookingIdAndTicketNumber Ticket.USED booking.id ticket.ticketNumber
