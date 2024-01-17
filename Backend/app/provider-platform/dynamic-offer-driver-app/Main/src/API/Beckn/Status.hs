{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Status (API, handler) where

import qualified Beckn.ACL.OnStatus as ACL
import qualified Beckn.ACL.Status as ACL
import qualified Beckn.Core as CallBAP
import qualified Beckn.OnDemand.Utils.Callback as Callback
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.API.Status as Status
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.Status as DStatus
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
    :> Status.StatusAPI

handler :: FlowServer API
handler = status

status ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  -- Status.StatusReq ->
  ByteString ->
  FlowHandler AckResponse
status transporterId (SignatureAuthResult _ subscriber) reqBS = withFlowHandlerBecknAPI do
  req <- decodeReq reqBS

  (dStatusReq, callbackUrl, bapId, msgId, city, country, txnId, bppId, bppUri) <- case req of
    Right reqV2 -> do
      transactionId <- Utils.getTransactionId reqV2.statusReqContext
      Utils.withTransactionIdLogTag transactionId $ do
        logTagInfo "Status APIV2 Flow" "Reached"
        dStatusReq <- ACL.buildStatusReqV2 subscriber reqV2
        let context = reqV2.statusReqContext
        callbackUrl <- Utils.getContextBapUri context
        bppUri <- Utils.getContextBppUri context
        messageId <- Utils.getMessageId context
        bapId <- Utils.getContextBapId context
        city <- Utils.getContextCity context
        country <- Utils.getContextCountry context
        pure (dStatusReq, callbackUrl, bapId, messageId, city, country, Just transactionId, context.contextBppId, bppUri)
    Left reqV1 ->
      withTransactionIdLogTag reqV1 $ do
        logTagInfo "Status API Flow" "Reached"
        dStatusReq <- ACL.buildStatusReq subscriber reqV1
        pure (dStatusReq, reqV1.context.bap_uri, reqV1.context.bap_id, reqV1.context.message_id, reqV1.context.city, reqV1.context.country, reqV1.context.transaction_id, reqV1.context.bpp_id, reqV1.context.bpp_uri)

  dStatusRes <- DStatus.handler transporterId dStatusReq
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then do
      context <- ContextV2.buildContextV2 Context.STATUS Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country
      Callback.withCallback dStatusRes.transporter "STATUS" OnStatus.onStatusAPIV2 callbackUrl internalEndPointHashMap (errHandler context) $
        pure $
          Spec.OnStatusReq
            { onStatusReqContext = context,
              onStatusReqError = Nothing,
              onStatusReqMessage = ACL.mkOnStatusMessageV2 dStatusRes
            }
    else do
      context <- buildTaxiContext Context.STATUS msgId txnId bapId callbackUrl bppId bppUri city country False
      CallBAP.withCallback dStatusRes.transporter Context.STATUS OnStatus.onStatusAPIV1 context callbackUrl internalEndPointHashMap $
        pure $ ACL.mkOnStatusMessage dStatusRes

decodeReq :: MonadFlow m => ByteString -> m (Either Status.StatusReq Status.StatusReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV2 -> pure $ Right reqV2
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV1 -> pure $ Left reqV1
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS

errHandler :: Spec.Context -> BecknAPIError -> Spec.OnStatusReq
errHandler context (BecknAPIError err) =
  Spec.OnStatusReq
    { onStatusReqContext = context,
      onStatusReqError = Just err',
      onStatusReqMessage = Nothing
    }
  where
    err' =
      Spec.Error
        { errorCode = Just err.code,
          errorMessage = err.message >>= \m -> Just $ encodeToText err._type <> " " <> m,
          errorPaths = err.path
        }
