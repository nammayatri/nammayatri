{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnInit (API, handler) where

import qualified Beckn.ACL.Bus.OnInit as BusACL
import qualified Beckn.ACL.Cancel as CancelACL
import qualified Beckn.ACL.Confirm as ACL
import qualified Beckn.ACL.OnInit as TaxiACL
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.ConfirmBus as DCB
import Domain.Types.CancellationReason
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Ticket as QRideT

type API = OnInit.OnInitAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onInit

onInit ::
  SignatureAuthResult ->
  OnInit.OnInitReq ->
  FlowHandler AckResponse
onInit _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnInitReq <- case req.context.domain of
    Context.MOBILITY -> TaxiACL.buildOnInitRideReq req
    Context.PUBLIC_TRANSPORT -> BusACL.buildOnInitBusReq req
    _ -> throwError (InvalidRequest $ "Unsupported Domain: " <> show req.context.domain)

  whenJust mbDOnInitReq $ \onInitReq ->
    case onInitReq.bppBookingId of
      Just bppBookingId -> do
        Redis.whenWithLockRedis (onInitLockKey bppBookingId.getId) 60 $
          fork "oninit request processing" $ do
            onInitRes <- DOnInit.onInit onInitReq
            let bookingId = fromJust onInitRes.bookingId
            booking <- QRideB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
            handle (errHandlerB booking) $
              void $ withShortRetry $ CallBPP.confirm onInitRes.bppUrl =<< ACL.buildConfirmReq onInitRes
      Nothing ->
        case onInitReq.ticketId of
          Just ticketId ->
            Redis.whenWithLockRedis (onInitBusLockKey ticketId.getId) 60 $
              fork "oninit request processing" $ do
                onInitRes <- DOnInit.onInit onInitReq
                ticket <- QRideT.findById ticketId >>= fromMaybeM (TicketDoesNotExist ticketId.getId)
                handle (errHandlerT ticket) $
                  void $ withShortRetry $ CallBPP.confirm onInitRes.bppUrl =<< ACL.buildConfirmBusReq onInitRes
          Nothing -> pure ()
  pure Ack
  where
    errHandlerB booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = do
        dCancelRes <- DCancel.cancel booking.id (booking.riderId, booking.merchantId) cancelReq
        void . withShortRetry $ CallBPP.cancel dCancelRes.bppUrl =<< CancelACL.buildCancelReq dCancelRes
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = do
        dCancelRes <- DCancel.cancel booking.id (booking.riderId, booking.merchantId) cancelReq
        void . withShortRetry $ CallBPP.cancel dCancelRes.bppUrl =<< CancelACL.buildCancelReq dCancelRes
      | otherwise = throwM exc
    errHandlerT ticket exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DCB.cancelTicket ticket
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DCB.cancelTicket ticket
      | otherwise = throwM exc
    cancelReq =
      DCancel.CancelReq
        { reasonCode = CancellationReasonCode "External/Beckn API failure",
          reasonStage = OnConfirm,
          additionalInfo = Nothing
        }

onInitLockKey :: Text -> Text
onInitLockKey id = "Customer:OnInit:BppBookingId-" <> id

onInitBusLockKey :: Text -> Text
onInitBusLockKey id = "Customer:OnInit:ticketId-" <> id
