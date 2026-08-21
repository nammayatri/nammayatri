{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnTrack (API, handler, onTrackWebhook) where

import qualified Beckn.ACL.OnTrack as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.OnTrack as DOnTrack
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.InboundGate as InboundGate
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error
import TransactionLogs.PushLogs

type API = OnTrack.OnTrackAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onTrack

onTrackWebhook :: OnTrack.OnTrackReqV2 -> FlowHandler AckResponse
onTrackWebhook = onTrack (error "OnTrack webhook: SignatureAuthResult not present (verified upstream by onix)")

onTrack ::
  SignatureAuthResult ->
  OnTrack.OnTrackReqV2 ->
  FlowHandler AckResponse
onTrack _ reqV2 = withFlowHandlerBecknAPI . ActorInfo.withRequestIdActorInfo $ do
  drop' <- do
    mbMerchant <- case reqV2.onTrackReqContext.contextBapId of
      Nothing -> pure Nothing
      Just bapId -> CQMerchant.findBySubscriberId (ShortId bapId)
    case mbMerchant of
      Nothing -> pure False
      Just merchant -> InboundGate.shouldDropSigned "on_track" merchant.id (reqV2.onTrackReqContext.contextBppId)
  if drop'
    then pure Ack
    else do
      transactionId <- Utils.getTransactionId reqV2.onTrackReqContext
      L.setOptionLocal TxnIdKey transactionId
      Utils.withTransactionIdLogTag transactionId $ do
        logTagInfo "onTrackAPIV2" $ "Received onTrack API call:-" <> show reqV2
        mbDOnTrackReq <- ACL.buildOnTrackReqV2 reqV2
        whenJust mbDOnTrackReq $ \onTrackReq -> do
          validatedReq <- DOnTrack.validateRequest onTrackReq
          fork "on track processing" $
            DOnTrack.onTrack validatedReq
          fork "on track received pushing ondc logs" do
            booking <- QBookingLite.findByIdLite validatedReq.ride.bookingId >>= fromMaybeM (BookingDoesNotExist $ "BookingId:-" <> validatedReq.ride.bookingId.getId)
            void $ pushLogs "on_track" (toJSON reqV2) booking.merchantId.getId "MOBILITY"
        pure Ack
