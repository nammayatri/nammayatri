module API.Init where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Core.Init
import Core.OnInit
import ExternalAPI (callBapOnInit)
import MockData.OnInit (buildOrderWithLogic)
import Types.App
import Utils

initServer :: BecknReq InitMessage -> MockM AckResponse
initServer confirmReq@(BecknReq ctx msg) = do
  mockLog INFO $ "got confirm request: " <> show confirmReq
  let incomeOrder = msg.order
  let eithOrder = buildOrderWithLogic incomeOrder

  _ <- mockFork $ do
    mockLog DEBUG "debug message inside fork"
    liftIO $ threadDelaySec 2
    context' <- buildOnSearchContext ctx
    let bapUri = ctx.bap_uri
        callbackData = either (Left . textToError) (Right . OnInitMessage) eithOrder
    ack <- callBapOnInit bapUri $ BecknCallbackReq context' callbackData
    mockLog DEBUG $ "got ack" <> show ack
  pure Ack

buildOnSearchContext :: Context -> MockM Context
buildOnSearchContext ctx = do
  now <- getCurrentTime
  bppId <- asks (.selfId)
  bppUri <- asks (.selfUri)
  let ctx' =
        ctx{action = ON_INIT,
            bpp_id = Just bppId,
            bpp_uri = Just bppUri,
            timestamp = now
           }
  pure ctx'
