module API.Search where

import API.Utils (buildOnActionContext)
import "public-transport-bap" Beckn.Spec.Search
import Environment
import ExternalAPI
import Kernel.Mock.App
import Kernel.Types.Beckn.Ack (AckResponse (..))
import Kernel.Types.Beckn.Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Common (logPretty)
import Kernel.Utils.Time
import MockData.OnSearch
import Relude

searchServer :: BecknReq SearchMessage -> MockM AppEnv AckResponse
searchServer becknReq@(BecknReq ctx req) = do
  logPretty DEBUG "request body" becknReq
  _ <- fork "call on_search" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    context' <- buildOnActionContext ON_SEARCH ctx
    let callbackData = onSearchCatalog req.intent.fulfillment.start.time.range.start
    _ <- callGatewayOnSearch $ BecknCallbackReq context' $ Right callbackData
    pure ()
  pure Ack
