module API.Search where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Control.Concurrent
import Core.Search
import ExternalAPI
import MockData.OnSearch
import Types.App

searchServer :: BecknReq SearchMessage -> MockM AckResponse
searchServer becknReq@(BecknReq ctx _) = do
  mockLog DEBUG $ "request body: " <> show becknReq
  _ <- mockFork $ do
    mockLog DEBUG "debug message inside fork"
    liftIO $ threadDelay $ 1000000 * 2
    context' <- buildOnSearchContext ctx
    let callbackData = onSearchCatalog
    ack <- callGatewayOnSearchS $ BecknCallbackReq context' $ Right callbackData
    mockLog DEBUG $ "got ack" <> show ack
  pure Ack

buildOnSearchContext :: Context -> MockM Context
buildOnSearchContext ctx = do
  now <- getCurrentTime
  bppId <- asks (.selfId)
  bppUri <- asks (.selfUri)
  let ctx' =
        ctx{action = ON_SEARCH,
            bpp_id = Just bppId,
            bpp_uri = Just bppUri,
            timestamp = now
           }
  pure ctx'
