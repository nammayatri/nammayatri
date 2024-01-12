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
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Merchant as DM
import Environment
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
import Storage.Beam.SystemConfigs ()

-- import qualified Beckn.OnDemand.Utils.Callback as Callback

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Init.InitAPIV1

handler :: FlowServer API
handler = init

init ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Init.InitReq ->
  -- ByteString ->
  FlowHandler AckResponse
init transporterId subscriber reqV1 =
  -- req <- decodeReq reqBS
  initV1Flow transporterId subscriber reqV1

-- dInitReq <-
--   case req of
--     Right reqV2 -> initV2Flow reqV2
--     Left reqV1 -> initV1Flow reqV1

initLockKey :: Text -> Text
initLockKey id = "Driver:Init:DriverQuoteId-" <> id

initProcessingLockKey :: Text -> Text
initProcessingLockKey id = "Driver:Init:Processing:DriverQuoteId-" <> id

initV1Flow :: Id DM.Merchant -> SignatureAuthResult -> Init.InitReq -> FlowHandler AckResponse
initV1Flow transporterId (SignatureAuthResult _ subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Init API Flow" "Reached"
    dInitReq <- ACL.buildInitReq subscriber req
    Redis.whenWithLockRedis (initLockKey dInitReq.estimateId) 60 $ do
      let context = req.context
      validatedRes <- DInit.validateRequest transporterId dInitReq
      fork "init request processing" $ do
        Redis.whenWithLockRedis (initProcessingLockKey dInitReq.estimateId) 60 $ do
          dInitRes <- DInit.handler transporterId dInitReq validatedRes
          internalEndPointHashMap <- asks (.internalEndPointHashMap)
          void . handle (errHandler dInitRes.booking) $
            CallBAP.withCallback dInitRes.transporter Context.INIT OnInit.onInitAPIV1 context context.bap_uri internalEndPointHashMap $
              pure $ ACL.mkOnInitMessage dInitRes
      return ()
    pure Ack
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DInit.cancelBooking booking transporterId
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DInit.cancelBooking booking transporterId
      | otherwise = throwM exc

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
--       | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DInit.cancelBooking booking transporterId
--       | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DInit.cancelBooking booking transporterId
--       | otherwise = throwM exc
