module API.Search where

import API.Utils (buildOnActionContext)
import Beckn.Mock.App
import Beckn.Mock.Environment
import Beckn.Mock.Utils
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Core.Search
import ExternalAPI
import MockData.OnSearch
import Relude

searchServer :: BecknReq SearchMessage -> MockM AppEnv AckResponse
searchServer becknReq@(BecknReq ctx _) = do
  mockLog DEBUG $ "request body: " <> show becknReq
  _ <- mockFork $ do
    mockLog DEBUG "debug message inside fork"
    threadDelaySec 2
    context' <- buildOnActionContext ON_SEARCH ctx
    let callbackData = onSearchCatalog
    ack <- callGatewayOnSearch $ BecknCallbackReq context' $ Right callbackData
    mockLog DEBUG $ "got ack" <> show ack
  pure Ack
