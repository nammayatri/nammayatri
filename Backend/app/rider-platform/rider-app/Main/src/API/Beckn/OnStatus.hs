{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnStatus (API, handler) where

import qualified Beckn.ACL.OnStatus as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Logging
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Booking as QRB
import TransactionLogs.PushLogs

type API = OnStatus.OnStatusAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onStatus

onStatus ::
  SignatureAuthResult ->
  OnStatus.OnStatusReqV2 ->
  FlowHandler AckResponse
onStatus _ reqV2 = withFlowHandlerBecknAPI $
  withDynamicLogLevel "rider-onstatus-api" $ do
    transactionId <- Utils.getTransactionId reqV2.onStatusReqContext
    Utils.withTransactionIdLogTag transactionId $ do
      logDebug $ "RIDER_ONSTATUS_API_DEBUG: Received on_status request for transactionId: " <> transactionId
      logTagError "onStatusAPIV2" $ "Received onStatus API call:-" <> show reqV2
      messageId <- Utils.getMessageIdText reqV2.onStatusReqContext
      mbDOnStatusReq <- ACL.buildOnStatusReqV2 reqV2 transactionId
      whenJust mbDOnStatusReq $ \onStatusReq -> do
        logDebug $ "RIDER_ONSTATUS_API_DEBUG: Processing on_status for transactionId: " <> transactionId <> " with bppBookingId: " <> onStatusReq.bppBookingId.getId
        Redis.whenWithLockRedis (onStatusLockKey messageId) 60 $ do
          validatedOnStatusReq <- DOnStatus.validateRequest onStatusReq
          fork "on status processing" $ do
            Redis.whenWithLockRedis (onStatusProcessngLockKey messageId) 60 $
              DOnStatus.onStatus validatedOnStatusReq
            fork "on status received pushing ondc logs" do
              logDebug $ "RIDER_ONSTATUS_API_DEBUG: Looking for booking with bppBookingId: " <> onStatusReq.bppBookingId.getId
              booking <- QRB.findByBPPBookingId onStatusReq.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> onStatusReq.bppBookingId.getId)
              logDebug $ "RIDER_ONSTATUS_API_DEBUG: Found booking: " <> booking.id.getId <> " with status: " <> show booking.status
              void $ pushLogs "on_status" (toJSON reqV2) booking.merchantId.getId "MOBILITY"
    pure Ack

onStatusLockKey :: Text -> Text
onStatusLockKey id = "Customer:OnStatus:MessageId-" <> id

onStatusProcessngLockKey :: Text -> Text
onStatusProcessngLockKey id = "Customer:OnStatus:Processing:MessageId-" <> id
