{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Track (API, handler) where

import qualified Beckn.ACL.OnTrack as ACL
import qualified Beckn.ACL.Track as ACL
import qualified Beckn.OnDemand.Utils.Callback as Callback
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Beckn.Types.Core.Taxi.API.Track as Track
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Domain.Action.Beckn.Track as DTrack
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.Booking as QRB
import TransactionLogs.PushLogs

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Track.TrackAPIV2

handler :: FlowServer API
handler = track

track ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Track.TrackReqV2 ->
  FlowHandler AckResponse
track transporterId (SignatureAuthResult _ subscriber) reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.trackReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "track APIV2 Flow" $ "Reached:-" <> show reqV2
    let txnId = Just transactionId
        trackContext = reqV2.trackReqContext
        bppId = trackContext.contextBppId
    dTrackReq <- ACL.buildTrackReqV2 subscriber reqV2
    callbackUrl <- Utils.getContextBapUri trackContext
    bppUri <- Utils.getContextBppUri trackContext
    msgId <- Utils.getMessageId trackContext
    bapId <- Utils.getContextBapId trackContext
    city <- Utils.getContextCity trackContext
    country <- Utils.getContextCountry trackContext

    dTrackRes <- DTrack.track transporterId dTrackReq

    internalEndPointHashMap <- asks (.internalEndPointHashMap)
    let becknOnTrackMessage = ACL.mkOnTrackMessageV2 dTrackRes
    logTagInfo "track APIV2 Flow" $ "Sending OnTrack APIV2" <> show becknOnTrackMessage
    booking <- QRB.findById dTrackReq.bookingId >>= fromMaybeM (BookingNotFound dTrackReq.bookingId.getId)
    let vehicleCategory = Utils.mapServiceTierToCategory booking.vehicleServiceTier
    bppConfig <- QBC.findByMerchantIdDomainAndVehicle transporterId (show Context.MOBILITY) vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
    fork "track received pushing ondc logs" do
      void $ pushLogs "track" (toJSON reqV2) transporterId.getId "MOBILITY"
    ttl <- bppConfig.onTrackTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
    onTrackContext <- ContextV2.buildContextV2 Context.ON_TRACK Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country (Just ttl)
    Callback.withCallback dTrackRes.transporter "on_track" OnTrack.onTrackAPIV2 callbackUrl internalEndPointHashMap (errHandler onTrackContext) $
      pure
        Spec.OnTrackReq
          { onTrackReqContext = onTrackContext,
            onTrackReqError = Nothing,
            onTrackReqMessage = Just becknOnTrackMessage
          }

errHandler :: Spec.Context -> BecknAPIError -> Spec.OnTrackReq
errHandler context (BecknAPIError err) =
  Spec.OnTrackReq
    { onTrackReqContext = context,
      onTrackReqError = Just err',
      onTrackReqMessage = Nothing
    }
  where
    err' =
      Spec.Error
        { errorCode = Just err.code,
          errorMessage = err.message >>= \m -> Just $ encodeToText err._type <> " " <> m,
          errorPaths = err.path
        }
