{-# LANGUAGE TypeApplications #-}

module API.Status where

import API.Utils
import qualified Control.Monad.Catch as C
import "public-transport-bap" Core.Spec.OnStatus
import "public-transport-bap" Core.Spec.Status
import Environment
import ExternalAPI
import Kernel.Mock.App
import Kernel.Mock.Utils
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Time
import qualified Redis
import Relude

statusServer :: BecknReq StatusMessage -> MockM AppEnv AckResponse
statusServer statusReq@(BecknReq ctx msg) = do
  logOutput INFO $ "got confirm request: " <> show statusReq
  context' <- buildOnActionContext ON_STATUS ctx
  let orderId = msg.order.id
  logOutput INFO $ "reading order with orderId=" <> orderId
  eithCtxOrd <- C.try @(MockM AppEnv) @SomeException (Redis.readOrder orderId)

  _ <- fork "call on_status" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    let eithOnStatusMsg = bimap (textToError . show) (OnStatusMessage . snd) eithCtxOrd
        onStatusReq = BecknCallbackReq context' eithOnStatusMsg
    callBapOnStatus onStatusReq
  pure Ack
