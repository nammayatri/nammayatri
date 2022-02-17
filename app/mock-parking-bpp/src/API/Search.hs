module API.Search where

import API.Utils (buildOnActionContext)
import Beckn.Mock.App
import Beckn.Mock.Utils
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Forkable
import Beckn.Utils.Logging
import Core.Search
import Environment
import ExternalAPI
import MockData.OnSearch
import Relude

searchServer :: BecknReq SearchIntent -> MockM AppEnv AckResponse
searchServer becknReq@(BecknReq ctx _) = do
  logDebug $ "request body: " <> show becknReq
  _ <- fork "call on_search" $ do
    waitMilliSec <- asks (.config.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    context' <- buildOnActionContext ON_SEARCH ctx
    let callbackData = onSearchCatalog
    _ <- callGatewayOnSearch $ BecknCallbackReq context' $ Right callbackData
    logDebug "got ack"
  pure Ack
