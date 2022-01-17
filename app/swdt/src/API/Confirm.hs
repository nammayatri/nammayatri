module API.Confirm where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Core.Confirm (ConfirmMessage)
import Types.App

confirmServer :: BecknReq ConfirmMessage -> MockM AckResponse
confirmServer _ = do
  mockLog INFO $ "got confirm request"
  pure Ack

{-
confirmServer :: BecknReq ConfirmMessage -> MockM AckResponse
confirmServer confirmReq@(BecknReq ctx req) = do
  mockLog DEBUG $ "request body: " <> show confirmReq
  _ <- mockFork $ do
    mockLog DEBUG "debug message inside fork"
    liftIO $ threadDelay $ 1000000 * 2
    context' <- buildOnConfirmContext ctx
    let onConfirmCallbackData = buildOnConfirmCallbackData req
    let bapUri = ctx.bap_uri
    ack <- callBapOnConfirmS bapUri $ BecknCallbackReq context' $ Right onConfirmCallbackData
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
-}
