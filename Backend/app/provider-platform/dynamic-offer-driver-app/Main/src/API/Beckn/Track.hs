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
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import BecknV2.Utils
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
import Tools.TransactionLogs
import TransactionLogs.Interface
import TransactionLogs.Interface.Types

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
    logTagInfo "track APIV2 Flow" "Reached"
    dTrackReq <- ACL.buildTrackReqV2 subscriber reqV2
    let context = reqV2.trackReqContext
    callbackUrl <- Utils.getContextBapUri context
    bppUri <- Utils.getContextBppUri context
    msgId <- Utils.getMessageId context
    bapId <- Utils.getContextBapId context
    city <- Utils.getContextCity context
    country <- Utils.getContextCountry context

    dTrackRes <- DTrack.track transporterId dTrackReq

    internalEndPointHashMap <- asks (.internalEndPointHashMap)
    fork "track received pushing ondc logs" do
      let kafkaLog = TransactionLog "on_search" $ Req reqV2.trackReqContext (toJSON reqV2.trackReqMessage)
      pushBecknLogToKafka kafkaLog
      let transactionLog = TransactionLogReq "track" $ ReqLog (toJSON reqV2.trackReqContext) (maskSensitiveData $ toJSON reqV2.trackReqMessage)
      becknConfig <- QBC.findByMerchantIdDomainAndVehicle transporterId "MOBILITY" (Utils.mapVariantToVehicle dTrackRes.booking.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
      void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
    logTagInfo "track APIV2 Flow" "Sending OnTrack APIV2"
    context' <- ContextV2.buildContextV2 Context.ON_TRACK Context.MOBILITY msgId (Just transactionId) bapId callbackUrl context.contextBppId bppUri city country (Just "PT2M")
    let onTrackBecknReq = mkOnTrackRequest context (ACL.mkOnTrackMessageV2 dTrackRes)
    fork "sending on track, pushing ondc logs" do
      let kafkaLog = TransactionLog "on_track" $ Req onTrackBecknReq.onTrackReqContext (toJSON onTrackBecknReq.onTrackReqMessage)
      pushBecknLogToKafka kafkaLog
      let transactionLog = TransactionLogReq "on_track" $ ReqLog (toJSON onTrackBecknReq.onTrackReqContext) (maskSensitiveData $ toJSON onTrackBecknReq.onTrackReqMessage)
      becknConfig <- QBC.findByMerchantIdDomainAndVehicle transporterId "MOBILITY" (Utils.mapVariantToVehicle dTrackRes.booking.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
      void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
    Callback.withCallback dTrackRes.transporter "TRACK" OnTrack.onTrackAPIV2 callbackUrl internalEndPointHashMap (errHandler context') $ pure onTrackBecknReq

mkOnTrackRequest :: Spec.Context -> Spec.OnTrackReqMessage -> Spec.OnTrackReq
mkOnTrackRequest context message =
  Spec.OnTrackReq
    { onTrackReqContext = context,
      onTrackReqError = Nothing,
      onTrackReqMessage = Just message
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
