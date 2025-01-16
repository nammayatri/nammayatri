{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.Webhook where

import qualified Control.Exception as CT
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Domain.Types.MerchantConfigs as MC
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (elem, filter, id, length, map, mapM_, whenJust)
import Kernel.Prelude
import Kernel.Utils.Common
import Network.HTTP.Client
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types (HeaderName)

sendWebHook :: [MC.MerchantConfigs] -> LBS.ByteString -> Environment.Flow ()
sendWebHook merchants webhookBody = do
  mapM_
    ( \receiver -> do
        let webHookHeaders = convertToHeaders receiver.webHookHeaders
        let request = parseRequest_ (T.unpack receiver.webHookUrl)
            request' =
              request
                { method = "POST",
                  requestBody = RequestBodyLBS webhookBody,
                  requestHeaders = webHookHeaders
                }
        manager <- liftIO getGlobalManager
        logDebug $ "Sending webhook to " <> show request'
        result <- liftIO $ CT.try $ httpLbs request' manager
        case result of
          Left (CT.SomeException e) -> do
            logError $ "Webhook failed for " <> show request' <> " with error: " <> show e
          Right out -> do
            logDebug $ "Webhook response: " <> show out
    )
    merchants
  where
    convertToHeaders :: [MC.WebHookHeaders] -> [(HeaderName, ByteString)]
    convertToHeaders = map (\header -> (CI.mk $ encodeUtf8 header.key, encodeUtf8 header.value))
