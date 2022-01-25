module API.Status where

import API.Confirm.Coerce
import API.Utils
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Common.App
import Common.Environment
import qualified Common.Redis as Redis
import Common.Utils
import Core.OnStatus
import Core.Status
import ExternalAPI
import Relude

statusServer :: BecknReq StatusMessage -> MockM AppEnv AckResponse
statusServer statusReq@(BecknReq ctx msg) = do
  mockLog INFO $ "got confirm request: " <> show statusReq
  context' <- buildOnActionContext ON_STATUS ctx
  let orderId = msg.order.id
  eithCtxOrd <- Redis.readCtxOrderEither orderId

  _ <- mockFork $ do
    threadDelaySec 2
    let eithOnStatusMsg = bimap textToError (OnStatusMessage . coerceOrderStatus . snd) eithCtxOrd
        onStatusReq = BecknCallbackReq context' eithOnStatusMsg
    callBapOnStatus onStatusReq
  pure Ack
