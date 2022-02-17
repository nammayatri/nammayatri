{-# LANGUAGE TypeApplications #-}

module API.Status where

import API.Utils (buildOnActionContext)
import Beckn.Mock.App
import Beckn.Mock.Utils
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Forkable
import Beckn.Utils.Logging
import qualified Control.Monad.Catch as C
import Core.OnStatus
import Core.Status
import Environment
import ExternalAPI
import qualified Redis
import Relude hiding (state)

statusServer :: BecknReq StatusMessage -> MockM AppEnv AckResponse
statusServer (BecknReq ctx msg) = do
  logInfo $ "got confirm request: " <> show msg
  context' <- buildOnActionContext ON_STATUS ctx
  let orderId = msg.order.id
  eithCtxOrd <- C.try @(MockM AppEnv) @SomeException (Redis.readOrder orderId)
  _ <- fork "call on_status" $ do
    waitMilliSec <- asks (.config.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    let eithOnStatusMsg = bimap (textToError . show) (OnStatusMessage . snd) eithCtxOrd
        onStatusReq = BecknCallbackReq context' eithOnStatusMsg
    callBapOnStatus onStatusReq
  pure Ack
