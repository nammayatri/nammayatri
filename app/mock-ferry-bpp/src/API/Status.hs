module API.Status where

import API.Confirm.Coerce
import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Core.OnStatus
import Core.Status
import ExternalAPI
import qualified Redis
import Types.App
import Utils

statusServer :: BecknReq StatusMessage -> MockM AckResponse
statusServer statusReq@(BecknReq ctx msg) = do
  mockLog INFO $ "got confirm request: " <> show statusReq
  context' <- buildOnStatusContext ctx
  let orderId = msg.order.id
  eithCtxOrd <- Redis.readCtxOrderEither orderId

  _ <- mockFork $
    (liftIO (threadDelaySec 2) >>) $ case eithCtxOrd of
      Left errMsg -> do
        let onStatusReq = BecknCallbackReq context' $ Left $ textToError errMsg
        callBapOnStatus onStatusReq
      Right (_, ordOC) -> do
        let onStatusOrder = coerceOrderStatus ordOC
            onStatusReq = BecknCallbackReq context' $ Right $ OnStatusMessage onStatusOrder
        callBapOnStatus onStatusReq
  pure Ack

buildOnStatusContext :: Context -> MockM Context
buildOnStatusContext ctx = do
  now <- getCurrentTime
  bppId <- asks (.selfId)
  bppUri <- asks (.selfUri)
  let ctx' =
        ctx{action = ON_STATUS,
            bpp_id = Just bppId,
            bpp_uri = Just bppUri,
            timestamp = now
           }
  pure ctx'
