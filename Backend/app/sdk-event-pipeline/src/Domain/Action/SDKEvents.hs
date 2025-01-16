{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.SDKEvents where

import qualified Data.Aeson as A
import Domain.Types.SDKEvents
import Environment
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer
import Kernel.Types.APISuccess
import Tools.Auth

postSdkEvents :: SDKEventsReq -> Flow APISuccess
postSdkEvents req = do
  --   apiRateLimitOptions <- asks (.apiRateLimitOptions)
  --   checkSlidingWindowLimitWithOptions ("rate-limit:" <> personId) apiRateLimitOptions
  let clientType = fromMaybe DRIVER req.clientType
      mbEvent :: Maybe A.Value = A.decode $ encodeUtf8 req.event
  whenJust mbEvent $ \event -> do
    case clientType of
      RIDER -> do
        riderSDKEventsKafkaTopic <- asks (.riderSDKEventsKafkaTopic)
        produceMessage (riderSDKEventsKafkaTopic, Nothing) event
      DRIVER -> do
        driverSDKEventsKafkaTopic <- asks (.driverSDKEventsKafkaTopic)
        produceMessage (driverSDKEventsKafkaTopic, Nothing) event
      METRO_WEBVIEW -> do
        metroWebviewEventsKafkaTopic <- asks (.metroWebviewEventsKafkaTopic)
        produceMessage (metroWebviewEventsKafkaTopic, Nothing) event
  return Success
