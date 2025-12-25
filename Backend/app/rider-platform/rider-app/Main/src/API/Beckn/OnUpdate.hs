{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnUpdate (API, handler) where

import qualified Beckn.ACL.OnUpdate as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as OnUpdate
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Booking as QRB
import Tools.Error
import TransactionLogs.PushLogs

type API = OnUpdate.OnUpdateAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onUpdate

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReqV2 ->
  FlowHandler AckResponse
onUpdate _ reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.onUpdateReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "onUpdateAPIV2" $ "Received onUpdate API call:-" <> show reqV2
    mbDOnUpdateReq <- ACL.buildOnUpdateReqV2 reqV2
    messageId <- Utils.getMessageIdText reqV2.onUpdateReqContext

    whenJust mbDOnUpdateReq $ \onUpdateReq ->
      Redis.whenWithLockRedis (onUpdateLockKey messageId) 60 $ do
        validatedOnUpdateReq <- DOnUpdate.validateRequest onUpdateReq
        fork "on update processing" $ do
          Redis.whenWithLockRedis (onUpdateProcessngLockKey messageId) 60 $
            DOnUpdate.onUpdate validatedOnUpdateReq
        fork "on update received pushing ondc logs" do
          booking <- case validatedOnUpdateReq of
            DOnUpdate.OUValidatedScheduledRideAssignedReq req -> QRB.findByBPPBookingId req.bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bookingDetails.bppBookingId.getId)
            DOnUpdate.OUValidatedRideAssignedReq req -> QRB.findByBPPBookingId req.bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bookingDetails.bppBookingId.getId)
            DOnUpdate.OUValidatedRideStartedReq req -> QRB.findByBPPBookingId req.bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bookingDetails.bppBookingId.getId)
            DOnUpdate.OUValidatedRideCompletedReq req -> QRB.findByBPPBookingId req.bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bookingDetails.bppBookingId.getId)
            DOnUpdate.OUValidatedBookingCancelledReq req -> QRB.findByBPPBookingId req.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bppBookingId.getId)
            DOnUpdate.OUValidatedBookingReallocationReq req -> return req.booking
            DOnUpdate.OUValidatedDriverArrivedReq req -> QRB.findByBPPBookingId req.bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bookingDetails.bppBookingId.getId)
            DOnUpdate.OUValidatedEstimateRepetitionReq req -> return req.booking
            DOnUpdate.OUValidatedQuoteRepetitionReq req -> return req.booking
            DOnUpdate.OUValidatedNewMessageReq req -> return req.booking
            DOnUpdate.OUValidatedSafetyAlertReq req -> return req.booking
            DOnUpdate.OUValidatedPhoneCallRequestEventReq req -> return req.booking
            DOnUpdate.OUValidatedPhoneCallCompletedEventReq req -> return req.booking
            DOnUpdate.OUValidatedStopArrivedReq req -> return req.booking
            DOnUpdate.OUValidatedFarePaidReq req -> return req.booking
            DOnUpdate.OUValidatedEditDestSoftUpdateReq req -> return req.booking
            DOnUpdate.OUValidatedEditDestConfirmUpdateReq req -> return req.booking
            DOnUpdate.OUValidatedTollCrossedEventReq req -> return req.booking
            DOnUpdate.OUValidatedDestinationReachedReq req -> return req.booking
            DOnUpdate.OUValidatedEstimatedEndTimeRangeReq req -> return req.booking
            DOnUpdate.OUValidatedParcelImageFileUploadReq req -> return req.booking
            DOnUpdate.OUValidatedEditDestError _ -> throwError $ InternalError "Error request is not supported for network observability"
          void $ pushLogs "on_update" (toJSON reqV2) booking.merchantId.getId "MOBILITY"
  pure Ack

onUpdateLockKey :: Text -> Text
onUpdateLockKey id = "Customer:OnUpdate:MessageId-" <> id

onUpdateProcessngLockKey :: Text -> Text
onUpdateProcessngLockKey id = "Customer:OnUpdate:Processing:MessageId-" <> id
