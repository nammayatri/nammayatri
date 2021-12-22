{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : FCM.Flow
-- Description : Firebase Cloud Messaging module
--
-- FCM description: https://firebase.google.com/docs/cloud-messaging
-- Firebase Cloud Messaging (FCM) is a cross-platform messaging solution
-- that lets you reliably send messages at no cost. Using FCM, you can notify
-- a client app that new email or other data is available to sync. You can
-- send notification messages to drive user re-engagement and retention.
-- For use cases such as instant messaging, a message can transfer
-- a payload of up to 4KB to a client app.
--
-- Protocol description : https://firebase.google.com/docs/reference/fcm/rest/v1/projects.messages
module Beckn.External.FCM.Flow where

import Beckn.External.FCM.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Common
import qualified Beckn.Utils.JWT as JWT
import Control.Exception (IOException)
import qualified Control.Exception as E (try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class
import EulerHS.Prelude hiding ((^.))
import qualified EulerHS.Types as ET
import Servant

-- | Create FCM message
-- Note that data should be formed as key-value pairs list
-- recipientId::FCMToken is an app's registration token
createMessage :: FCMAndroidData -> FCMRecipientToken -> FCMMessage
createMessage msgData recipientId =
  def{fcmToken = Just recipientId,
      fcmAndroid = Just androidCfg
     }
  where
    androidCfg = createAndroidConfig msgData

-- | Android Notification details
createAndroidConfig :: FCMAndroidData -> FCMAndroidConfig
createAndroidConfig cfgData =
  def{fcmdData = Just cfgData
     }

createAndroidNotification :: FCMNotificationTitle -> FCMNotificationBody -> FCMNotificationType -> FCMAndroidNotification
createAndroidNotification title body notificationType =
  let notification = case notificationType of
        ALLOCATION_REQUEST ->
          def{fcmdSound = Just "notify_sound.mp3",
              fcmdChannelId = Just "RINGING_ALERT"
             }
        TRIP_STARTED ->
          def{fcmdSound = Just "notify_otp_sound.mp3",
              fcmdChannelId = Just "TRIP_STARTED"
             }
        _ -> def
   in notification
        { fcmdTitle = Just title,
          fcmdBody = Just body,
          fcmdIcon =
            Just $
              FCMNotificationIconUrl
                "https://api.sandbox.beckn.juspay.in/static/images/ride-success.png",
          fcmdTag = Just notificationType
        }

-- | Send FCM message to a person
notifyPerson ::
  ( CoreMetrics m,
    FCMFlow m r
  ) =>
  FCMAndroidData ->
  FCMNotificationRecipient ->
  m ()
notifyPerson msgData recipient = do
  let tokenNotFound = "device token of a person " <> recipient.id <> " not found"
  case recipient.token of
    Nothing -> do
      logTagInfo "FCM" tokenNotFound
      pure ()
    Just token -> sendMessage (FCMRequest (createMessage msgData token)) recipient.id

-- | Google API interface
type FCMSendMessageAPI =
  Header "Authorization" FCMAuthToken
    :> ReqBody '[JSON] FCMRequest
    :> Post '[JSON] FCMResponse

fcmSendMessageAPI :: Proxy FCMSendMessageAPI
fcmSendMessageAPI = Proxy

-- | Send FCM message to a registered device
sendMessage ::
  ( CoreMetrics m,
    FCMFlow m r
  ) =>
  FCMRequest ->
  Text ->
  m ()
sendMessage fcmMsg toWhom = fork desc $ do
  authToken <- getTokenText
  case authToken of
    Right token -> do
      fcmUrl <- asks (.fcmUrl)
      res <- callAPI fcmUrl (callFCM (Just $ FCMAuthToken token) fcmMsg) "sendMessage"
      case res of
        Right _ -> logTagInfo fcm $ "message sent successfully to a person with id " <> toWhom
        Left x -> logTagError fcm $ "error while sending message to person with id " <> toWhom <> " : " <> show x
    Left err -> do
      logTagError fcm $ "error while sending message to person with id " <> toWhom <> " : " <> show err
  where
    callFCM token msg = void $ ET.client fcmSendMessageAPI token msg
    desc = "FCM send message forked flow"
    fcm = "FCM"

-- | try to get FCM text token
getTokenText ::
  FCMFlow m r =>
  m (Either Text Text)
getTokenText = do
  token <- getToken
  pure $ case token of
    Left err -> Left $ fromString err
    Right t -> Right $ JWT.jwtTokenType t <> " " <> JWT.jwtAccessToken t

-- | Get token (refresh token if expired / invalid)
getToken ::
  FCMFlow m r =>
  m (Either String JWT.JWToken)
getToken = do
  tokenStatus <-
    Redis.getKeyRedis "beckn:fcm_token" >>= \case
      Nothing -> pure $ Left "Token not found"
      Just jwt -> do
        validityStatus <- liftIO $ JWT.isValid jwt
        pure $ case validityStatus of
          JWT.JWTValid _ -> Right jwt
          JWT.JWTExpired _ -> Left "Token expired"
          JWT.JWTInvalid -> Left "Token is invalid"
  case tokenStatus of
    Left err -> do
      logTagWarning "FCM" $ "Refreshing FCM token. Reason: " <> fromString err
      getNewToken
    jwt -> pure jwt

getAndParseFCMAccount ::
  FCMFlow m r =>
  m (Either String JWT.ServiceAccount)
getAndParseFCMAccount = do
  mbFcmFile <- asks (.fcmJsonPath)
  case mbFcmFile of
    Nothing -> pure $ Left "FCM JSON file is not set in configs"
    Just fcmFile -> do
      rawContent <- liftIO . E.try @IOException . BL.readFile $ toString fcmFile
      pure $ parseContent $ first show rawContent
  where
    parseContent :: Either String BL.ByteString -> Either String JWT.ServiceAccount
    parseContent rawContent = rawContent >>= Aeson.eitherDecode

getNewToken :: FCMFlow m r => m (Either String JWT.JWToken)
getNewToken = getAndParseFCMAccount >>= either (pure . Left) refreshToken

refreshToken :: MonadFlow m => JWT.ServiceAccount -> m (Either String JWT.JWToken)
refreshToken fcmAcc = do
  logTagInfo fcmTag "Refreshing token"
  refreshRes <- liftIO $ JWT.doRefreshToken fcmAcc
  case refreshRes of
    Left err -> do
      logTagInfo fcmTag $ fromString err
      pure $ Left $ fromString err
    Right token -> do
      logTagInfo fcmTag $ fromString "Success"
      Redis.setKeyRedis "beckn:fcm_token" token
      pure $ Right token
  where
    fcmTag = "FCM"
