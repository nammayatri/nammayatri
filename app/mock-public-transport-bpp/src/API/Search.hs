module API.Search where

import API.Utils (buildOnActionContext)
import Beckn.Mock.App
import Beckn.Mock.Utils
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Core.Search
import Environment
import ExternalAPI
import MockData.OnSearch
import Relude

searchServer :: BecknReq SearchMessage -> MockM AppEnv AckResponse
searchServer becknReq@(BecknReq ctx _) = do
  mockLog DEBUG $ "request body: " <> show becknReq
  _ <- mockFork $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    context' <- buildOnActionContext ON_SEARCH ctx
    let callbackData = onSearchCatalog
    _ <- callGatewayOnSearch $ BecknCallbackReq context' $ Right callbackData
    pure ()
  pure Ack
