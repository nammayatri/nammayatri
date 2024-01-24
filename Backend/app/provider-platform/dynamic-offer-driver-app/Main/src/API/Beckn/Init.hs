{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Init (API, handler) where

import qualified Beckn.ACL.Init as ACL
import qualified Beckn.ACL.OnInit as ACL
import qualified Beckn.Core as CallBAP
import qualified Beckn.OnDemand.Utils.Callback as Callback
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude hiding (init)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified SharedLogic.Booking as SBooking
import Storage.Beam.SystemConfigs ()

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Init.InitAPI

handler :: FlowServer API
handler = init

init ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  -- Init.InitReq ->
  ByteString ->
  FlowHandler AckResponse
init transporterId (SignatureAuthResult _ subscriber) reqBS = withFlowHandlerBecknAPI $ do
  req <- decodeReq reqBS
  (dInitReq, bapUri, bapId, msgId, city, country, txnId, bppId, bppUri) <- case req of
    Right reqV2 -> do
      transactionId <- Utils.getTransactionId reqV2.initReqContext
      Utils.withTransactionIdLogTag transactionId $ do
        logTagInfo "Init APIV2 Flow" "Reached"
        dInitReq <- ACL.buildInitReqV2 subscriber reqV2
        let context = reqV2.initReqContext
        callbackUrl <- Utils.getContextBapUri context
        bppUri <- Utils.getContextBppUri context
        messageId <- Utils.getMessageId context
        bapId <- Utils.getContextBapId context
        city <- Utils.getContextCity context
        country <- Utils.getContextCountry context
        pure (dInitReq, callbackUrl, bapId, messageId, city, country, Just transactionId, context.contextBppId, bppUri)
    Left reqV1 ->
      withTransactionIdLogTag reqV1 $ do
        logTagInfo "Init API Flow" "Reached"
        dInitReq <- ACL.buildInitReq subscriber reqV1
        pure (dInitReq, reqV1.context.bap_uri, reqV1.context.bap_id, reqV1.context.message_id, reqV1.context.city, reqV1.context.country, reqV1.context.transaction_id, reqV1.context.bpp_id, reqV1.context.bpp_uri)

  let initFulfillmentId =
        case dInitReq.fulfillmentId of
          DInit.EstimateId (Id fId) -> fId
          DInit.QuoteId (Id fId) -> fId

  Redis.whenWithLockRedis (initLockKey initFulfillmentId) 60 $ do
    validatedRes <- DInit.validateRequest transporterId dInitReq
    fork "init request processing" $ do
      Redis.whenWithLockRedis (initProcessingLockKey initFulfillmentId) 60 $ do
        dInitRes <- DInit.handler transporterId dInitReq validatedRes
        internalEndPointHashMap <- asks (.internalEndPointHashMap)
        isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
        if isBecknSpecVersion2
          then do
            context <- ContextV2.buildContextV2 Context.ON_INIT Context.MOBILITY msgId txnId bapId bapUri bppId bppUri city country
            void . handle (errHandler dInitRes.booking dInitRes.transporter) $
              Callback.withCallback dInitRes.transporter "INIT" OnInit.onInitAPIV2 bapUri internalEndPointHashMap (errHandlerV2 context) $ do
                pure $
                  Spec.OnInitReq
                    { onInitReqContext = context,
                      onInitReqError = Nothing,
                      onInitReqMessage = Just $ ACL.mkOnInitMessageV2 dInitRes
                    }
          else do
            context <- buildTaxiContext Context.ON_SELECT msgId txnId bapId bapUri bppId bppUri city country False
            void . handle (errHandler dInitRes.booking dInitRes.transporter) $
              CallBAP.withCallback dInitRes.transporter Context.INIT OnInit.onInitAPIV1 context context.bap_uri internalEndPointHashMap $
                pure $ ACL.mkOnInitMessage dInitRes
  pure Ack
  where
    errHandler booking transporter exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = SBooking.cancelBooking booking Nothing transporter >> pure Ack
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = SBooking.cancelBooking booking Nothing transporter >> pure Ack
      | otherwise = throwM exc

initLockKey :: Text -> Text
initLockKey id = "Driver:Init:DriverQuoteId-" <> id

initProcessingLockKey :: Text -> Text
initProcessingLockKey id = "Driver:Init:Processing:DriverQuoteId-" <> id

-- initV1Flow :: Id DM.Merchant -> SignatureAuthResult -> Init.InitReq -> FlowHandler AckResponse
-- initV1Flow transporterId (SignatureAuthResult _ subscriber) req =
--   withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
--     logTagInfo "Init API Flow" "Reached"
--     dInitReq <- ACL.buildInitReq subscriber req
--     Redis.whenWithLockRedis (initLockKey dInitReq.estimateId) 60 $ do
--       let context = req.context
--       validatedRes <- DInit.validateRequest transporterId dInitReq
--       fork "init request processing" $ do
--         Redis.whenWithLockRedis (initProcessingLockKey dInitReq.estimateId) 60 $ do
--           dInitRes <- DInit.handler transporterId dInitReq validatedRes
--           internalEndPointHashMap <- asks (.internalEndPointHashMap)
--           void . handle (errHandler dInitRes.booking) $
--             CallBAP.withCallback dInitRes.transporter Context.INIT OnInit.onInitAPIV1 context context.bap_uri internalEndPointHashMap $
--               pure $ ACL.mkOnInitMessage dInitRes
--       return ()
--     pure Ack
--   where
--     errHandler booking exc
--       | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = SBooking.cancelBooking booking transporterId
--       | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = SBooking.cancelBooking booking transporterId
--       | otherwise = throwM exc

-- _initV2Flow :: Id DM.Merchant -> SignatureAuthResult -> Init.InitReqV2 -> FlowHandler AckResponse
-- _initV2Flow transporterId (SignatureAuthResult _ subscriber) req = do
--   withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
--     logTagInfo "Init API Flow" "Reached"
--     dInitReq <- ACL.buildInitReqV2 subscriber req
--     Redis.whenWithLockRedis (initLockKey dInitReq.estimateId) 60 $ do
--       let context = req.initReqContext
--       validatedRes <- DInit.validateRequest transporterId dInitReq
--       fork "init request processing" $ do
--         Redis.whenWithLockRedis (initProcessingLockKey dInitReq.estimateId) 60 $ do
--           dInitRes <- DInit.handler transporterId dInitReq validatedRes
--           internalEndPointHashMap <- asks (.internalEndPointHashMap)
--           void . handle (errHandler dInitRes.booking) $ do
--             bap_url <- fromMaybeM (InvalidRequest "bap_uri not found") context.contextBapUri >>= parseBaseUrl
--             Callback.withCallback dInitRes.transporter "INIT" OnInit.onInitAPIV2 context bap_url internalEndPointHashMap $
--               pure $ ACL.mkOnInitMessageV2 dInitRes
--       return ()
--     pure Ack
--   where
--     errHandler booking exc
--       | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = SBooking.cancelBooking booking transporterId
--       | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = SBooking.cancelBooking booking transporterId
--       | otherwise = throwM exc

decodeReq :: MonadFlow m => ByteString -> m (Either Init.InitReq Init.InitReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV1 -> pure $ Left reqV1
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV2 -> pure $ Right reqV2
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS

errHandlerV2 :: Spec.Context -> BecknAPIError -> Spec.OnInitReq
errHandlerV2 context (BecknAPIError err) =
  Spec.OnInitReq
    { onInitReqContext = context,
      onInitReqError = Just err',
      onInitReqMessage = Nothing
    }
  where
    err' =
      Spec.Error
        { errorCode = Just err.code,
          errorMessage = err.message >>= \m -> Just $ encodeToText err._type <> " " <> m,
          errorPaths = err.path
        }
