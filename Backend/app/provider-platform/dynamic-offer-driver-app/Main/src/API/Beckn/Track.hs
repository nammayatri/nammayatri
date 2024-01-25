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
import Beckn.Core (withCallback')
import qualified Beckn.OnDemand.Utils.Callback as Callback
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Beckn.Types.Core.Taxi.API.Track as Track
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.Track as DTrack
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import Tools.Error (GenericError (InvalidRequest))

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Track.TrackAPI

handler :: FlowServer API
handler = track

track ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  -- Track.TrackReq ->
  ByteString ->
  FlowHandler AckResponse
track transporterId (SignatureAuthResult _ subscriber) reqBS = withFlowHandlerBecknAPI do
  req <- decodeReq reqBS
  (dTrackReq, callbackUrl, bapId, msgId, city, country, txnId, bppId, bppUri) <- case req of
    Right reqV2 -> do
      transactionId <- Utils.getTransactionId reqV2.trackReqContext
      Utils.withTransactionIdLogTag transactionId $ do
        logTagInfo "track APIV2 Flow" "Reached"
        dTrackReq <- ACL.buildTrackReqV2 subscriber reqV2
        let context = reqV2.trackReqContext
        callbackUrl <- Utils.getContextBapUri context
        bppUri <- Utils.getContextBppUri context
        messageId <- Utils.getMessageId context
        bapId <- Utils.getContextBapId context
        city <- Utils.getContextCity context
        country <- Utils.getContextCountry context
        pure (dTrackReq, callbackUrl, bapId, messageId, city, country, Just transactionId, context.contextBppId, bppUri)
    Left reqV1 -> do
      logTagInfo "track API Flow" "Reached"
      dTrackReq <- ACL.buildTrackReq subscriber reqV1
      pure (dTrackReq, reqV1.context.bap_uri, reqV1.context.bap_id, reqV1.context.message_id, reqV1.context.city, reqV1.context.country, reqV1.context.transaction_id, reqV1.context.bpp_id, reqV1.context.bpp_uri)

  dTrackRes <- DTrack.track transporterId dTrackReq

  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then do
      logTagInfo "track APIV2 Flow" "Sending OnTrack APIV2"
      context <- ContextV2.buildContextV2 Context.ON_TRACK Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country
      Callback.withCallback dTrackRes.transporter "TRACK" OnTrack.onTrackAPIV2 callbackUrl internalEndPointHashMap (errHandler context) $
        -- there should be DOnTrack.onTrack, but it is empty anyway
        pure $
          Spec.OnTrackReq
            { onTrackReqContext = context,
              onTrackReqError = Nothing,
              onTrackReqMessage = Just $ ACL.mkOnTrackMessageV2 dTrackRes
            }
    else do
      logTagInfo "track API Flow" "Sending OnTrack API"
      context <- buildTaxiContext Context.SEARCH msgId txnId bapId callbackUrl bppId bppUri city country False
      withCallback' withShortRetry dTrackRes.transporter Context.TRACK OnTrack.onTrackAPIV1 context callbackUrl internalEndPointHashMap $
        -- there should be DOnTrack.onTrack, but it is empty anyway
        pure $ ACL.mkOnTrackMessage dTrackRes

decodeReq :: MonadFlow m => ByteString -> m (Either Track.TrackReq Track.TrackReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV2 -> pure $ Right reqV2
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV1 -> pure $ Left reqV1
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS

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
