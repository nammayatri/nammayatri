module API.Init where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Error
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Control.Concurrent
import Core.Init.Order
import Core.OnInit.Order
import ExternalAPI (callBapOnInit)
import MockData.OnInit (buildOrderWithLogic)
import Types.App

initServer :: BecknReq InitMessage -> MockM AckResponse
initServer confirmReq@(BecknReq ctx msg) = do
  mockLog INFO $ "got confirm request: " <> show confirmReq
  let incomeOrder = msg.order
  let eithOrder = buildOrderWithLogic incomeOrder

  _ <- mockFork $ do
    mockLog DEBUG "debug message inside fork"
    liftIO $ threadDelay $ 1000000 * 2
    context' <- buildOnSearchContext ctx
    let bapUri = ctx.bap_uri
        callbackData = either (Left . textToError) (Right . OnInitMessage) eithOrder
    ack <- callBapOnInit bapUri $ BecknCallbackReq context' callbackData
    mockLog DEBUG $ "got ack" <> show ack
  pure Ack

textToError :: Text -> Error
textToError desc =
  Error
    { _type = CORE_ERROR,
      code = "400",
      path = Nothing,
      message = Just desc
    }

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
