module API.Confirm where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Control.Concurrent
import Core1.Confirm
import Core1.OnConfirm
import ExternalAPI
import MockData.OnConfirm
import Types.App
import Utils

confirmServer :: BecknReq ConfirmMessage -> MockM AckResponse
confirmServer confirmReq@(BecknReq ctx msg) = do
  mockLog INFO $ "got confirm request: " <> show confirmReq

  _ <- mockFork $ do
    mockLog DEBUG "debug message inside fork"
    liftIO $ threadDelay $ 1000000 * 2
    context' <- buildOnConfirmContext ctx
    let bapUri = ctx.bap_uri
        eithOrder = buildOnConfirmOrder msg.order
        callbackData = either (Left . textToError) (Right . OnConfirmMessage) eithOrder
    ack <- callBapOnConfirmS bapUri $ BecknCallbackReq context' callbackData
    mockLog DEBUG $ "got ack" <> show ack
  pure Ack

buildOnConfirmContext :: Context -> MockM Context
buildOnConfirmContext ctx = do
  now <- getCurrentTime
  bppId <- asks (.selfId)
  bppUri <- asks (.selfUri)
  let ctx' =
        ctx{action = ON_CONFIRM,
            bpp_id = Just bppId,
            bpp_uri = Just bppUri,
            timestamp = now
           }
  pure ctx'
