{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Beckn.External.FCM.Flow where

import Beckn.External.FCM.Types
import Beckn.Types.Storage.Person as Person
import Control.Lens
import Data.Default.Class
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding ((^.))
import qualified EulerHS.Types as ET
import Servant
import Servant.Client
import System.Environment

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

-- | Send FCM message to a person
notifyPersonTmp :: (Text -> L.Flow (Maybe Text)) -> Text -> Text -> FCMData -> Person -> L.Flow (Either Text ())
notifyPersonTmp f title body msgData person =
  case Person._deviceToken person of
    Nothing -> do
      L.logInfo (T.pack "FCM") $ "device token of a person " <> show (Person._id person) <> "not found"
      pure $ Left $ "device token of a person " <> show (Person._id person) <> "not found"
    Just token ->
      sendMessageTmp f $ FCMRequest $ createMessage title body msgData token

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
  authToken <- L.runIO $ FCMAuthToken . T.pack . ("Bearer " <>) <$> getEnv "FCM_AUTH_TOKEN"
  res <- L.callAPI defaultBaseUrl $ callFCM (Just authToken) fcmMsg
  L.logInfo (T.pack "FCM") $ case res of
    Right _ -> "message sent successfully to" <> ""
    Left x -> "error: " <> show x
  return $ first show res
  where
    callFCM token msg = void $ ET.client fcmSendMessageAPI token msg

-- | Send FCM message to a registered device
sendMessageTmp :: (Text -> L.Flow (Maybe Text)) -> FCMRequest -> L.Flow (Either Text ())
sendMessageTmp f fcmMsg = do
  -- authToken <- L.runIO $ FCMAuthToken . T.pack . ("Bearer " <>) <$> getEnv "FCM_AUTH_TOKEN"
  -- token <- f "fcm_auth_token"
  authToken <- FCMAuthToken . ("Bearer " <>) . fromMaybe "" <$> f "fcm_auth_token"
  res <- L.callAPI defaultBaseUrl $ callFCM (Just authToken) fcmMsg
  L.logInfo (T.pack "FCM") $ case res of
    Right _ -> "message sent successfully to" <> ""
    Left x -> "error: " <> show x
  return $ first show res
  where
    callFCM token msg = void $ ET.client fcmSendMessageAPI token msg
