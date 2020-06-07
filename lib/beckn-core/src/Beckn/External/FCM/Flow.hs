{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Beckn.External.FCM.Flow where

import Beckn.External.FCM.Types
import Beckn.Types.Storage.Person as Person
import Beckn.Utils.JWT as JWT
import Control.Lens
import Data.Default.Class
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding ((^.))
import qualified EulerHS.Types as ET
import Servant
import Servant.Client

-- | Create FCM message
-- Note that data should be formed as key-value pairs list
-- recipientId::FCMToken is an app's registration token
createMessage :: Text -> Text -> FCMData -> FCMRecipientToken -> FCMMessage
createMessage title body msgData recipientId =
  def & fcmToken ?~ getFCMRecipientToken recipientId
    & fcmData ?~ msgData
    & fcmAndroid ?~ androidCfg
  where
    tag = msgData ^. fcmNotificationType
    androidCfg = createAndroidConfig title body tag

-- | Android Notification details
createAndroidConfig :: Text -> Text -> Text -> FCMAndroidConfig
createAndroidConfig title body tag =
  def & fcmdNotification ?~ notification
  where
    notification =
      def & fcmdTitle ?~ title
        & fcmdBody ?~ body
        & fcmdIcon ?~ "https://api.sandbox.beckn.juspay.in/static/images/ride-success.png"
        & fcmdTag ?~ tag

-- | Send FCM message to a person
notifyPerson :: Text -> Text -> FCMData -> Person -> L.Flow (Either Text ())
notifyPerson title body msgData person =
  case Person._deviceToken person of
    Nothing -> do
      L.logInfo (T.pack "FCM") $ "device token of a person " <> show (Person._id person) <> "not found"
      pure $ Left $ "device token of a person " <> show (Person._id person) <> "not found"
    Just token ->
      sendMessage $ FCMRequest $ createMessage title body msgData token

type FCMSendMessageAPI =
  Header "Authorization" FCMAuthToken
    :> ReqBody '[JSON] FCMRequest
    :> Post '[JSON] FCMResponse

fcmSendMessageAPI :: Proxy FCMSendMessageAPI
fcmSendMessageAPI = Proxy

defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "fcm.googleapis.com",
      baseUrlPort = 443,
      baseUrlPath = "/v1/projects/jp-beckn-dev/messages:send"
    }

-- | Send FCM message to a registered device
sendMessage :: FCMRequest -> L.Flow (Either Text ())
sendMessage fcmMsg = do
  authToken <- L.runIO JWT.getToken
  case authToken of
    Right token -> do
      res <- L.callAPI defaultBaseUrl $ callFCM (Just $ FCMAuthToken token) fcmMsg
      L.logInfo (T.pack "FCM") $ case res of
        Right _ -> "message sent successfully to" <> ""
        Left x -> "error: " <> show x
      pure $ first show res
    Left err -> do
      L.logInfo (T.pack "JWT") $ "error: " <> show err
      pure $ Left (T.pack err)
  where
    callFCM token msg = void $ ET.client fcmSendMessageAPI token msg
