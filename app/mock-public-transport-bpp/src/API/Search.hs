module API.Search where

import API.Utils (buildOnActionContext)
import Beckn.Mock.App
import Beckn.Mock.Utils
import Beckn.Types.Common
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common (logPretty)
import Core.Search
import Environment
import ExternalAPI
import MockData.OnSearch
import Relude

searchServer :: BecknReq SearchMessage -> MockM AppEnv AckResponse
searchServer becknReq@(BecknReq ctx req) = do
  logPretty DEBUG "request body" becknReq
  _ <- fork "call on_search" $ do
    waitMilliSec <- asks (.config.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    context' <- buildOnActionContext ON_SEARCH ctx
    let callbackData = onSearchCatalog req.intent.fulfillment.start.time.range.start
    _ <- callGatewayOnSearch $ BecknCallbackReq context' $ Right callbackData
    pure ()
  pure Ack
