{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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
module Lib.FCM.Flow
  ( createMessage,
    createAndroidNotification,
    notifyPerson,
    notifyPersonWithPriority,
    FCMSendMessageAPI,
    fcmSendMessageAPI,
    parseFCMAccount,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Base64 as B64
import EulerHS.Prelude hiding ((^.))
import qualified EulerHS.Types as ET
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Kernel.Utils.JWT as JWT
import Lib.FCM.Types
import Servant

-- | Create FCM message
-- Note that data should be formed as key-value pairs list
-- recipientId::FCMToken is an app's registration token
createMessage :: (Default a) => FCMData a -> FCMRecipientToken -> Maybe FCMAndroidMessagePriority -> FCMMessage a
createMessage msgData recipientId priority =
  def{fcmToken = Just recipientId,
      fcmAndroid = Just androidCfg,
      fcmApns = Just apnsCfg
     }
  where
    androidCfg = createAndroidConfig msgData priority
    apnsCfg = createApnsConfig msgData

-- | Android Notification details
createAndroidConfig :: (Default a) => FCMData a -> Maybe FCMAndroidMessagePriority -> FCMAndroidConfig a
createAndroidConfig cfgData priority =
  def{fcmdData = Just cfgData,
      fcmdPriority = priority
     }

createApnsConfig :: FCMData a -> FCMApnsConfig a
createApnsConfig androidFcmData =
  def{fcmaPayload = Just apnsPayload,
      fcmaHeaders =
        Just
          ( def{fcmApnsPriority = Just "10"
               }
          )
     }
  where
    apnsPayload = createApnsPayload androidFcmData

createApnsPayload :: forall a. FCMData a -> FCMApnPayload a
createApnsPayload androidData =
  def {fcmAps = Just fcmAps}
  where
    fcmAlert :: FCMAlert
    fcmAlert =
      def{fcmBody = (.getFCMNotificationBody) <$> body,
          fcmTitle = (.getFCMNotificationTitle) <$> title
         }
    fcmAps :: FCMaps a
    fcmAps =
      def{fcmAlert = Just fcmAlert,
          fcmData = Just androidData,
          fcmCategory = Just androidData.fcmNotificationType
         }
    title :: Maybe FCMNotificationTitle
    title = androidData.fcmNotificationJSON.fcmdTitle

    body :: Maybe FCMNotificationBody
    body = androidData.fcmNotificationJSON.fcmdBody

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
                "http://localhost:8080/static/images/ride-success.png",
          fcmdTag = Just notificationType
        }

-- | Send FCM message to a person
notifyPerson ::
  ( CoreMetrics m,
    Default a,
    ToJSON a,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  FCMConfig ->
  FCMData a ->
  FCMNotificationRecipient ->
  m ()
notifyPerson config = notifyPersonWithPriority config Nothing

notifyPersonWithPriority ::
  ( CoreMetrics m,
    Default a,
    ToJSON a,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  FCMConfig ->
  Maybe FCMAndroidMessagePriority ->
  FCMData a ->
  FCMNotificationRecipient ->
  m ()
notifyPersonWithPriority config priority msgData recipient = do
  let tokenNotFound = "device token of a person " <> recipient.id <> " not found"
  case recipient.token of
    Nothing -> do
      logTagInfo "FCM" tokenNotFound
      pure ()
    Just token -> sendMessage config (FCMRequest (createMessage msgData token priority)) recipient.id

-- | Google API interface
type FCMSendMessageAPI a =
  Header "Authorization" FCMAuthToken
    :> ReqBody '[JSON] (FCMRequest a)
    :> Post '[JSON] FCMResponse

fcmSendMessageAPI :: Proxy (FCMSendMessageAPI a)
fcmSendMessageAPI = Proxy

-- | Send FCM message to a registered device
sendMessage ::
  ( CoreMetrics m,
    ToJSON a,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  FCMConfig ->
  FCMRequest a ->
  Text ->
  m ()
sendMessage config fcmMsg toWhom = fork desc $ do
  authToken <- getTokenText config
  case authToken of
    Right token -> do
      let fcmUrl = config.fcmUrl
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
  (Redis.HedisFlow m r, MonadFlow m) =>
  FCMConfig ->
  m (Either Text Text)
getTokenText config = do
  token <- getToken config
  pure $ case token of
    Left err -> Left $ fromString err
    Right t -> Right $ JWT.jwtTokenType t <> " " <> JWT.jwtAccessToken t

redisFcmKey :: Text
redisFcmKey = "mobility:fcm_token"

-- | Get token (refresh token if expired / invalid)
getToken ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  FCMConfig ->
  m (Either String JWT.JWToken)
getToken config = do
  tokenStatus <-
    Redis.get (config.fcmTokenKeyPrefix <> ":" <> redisFcmKey) >>= \case
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
      getNewToken config
    jwt -> pure jwt

parseFCMAccount ::
  Text ->
  Either String JWT.ServiceAccount
parseFCMAccount fcmServiceAccount = do
  case BL.fromStrict . T.encodeUtf8 <$> B64.decodeBase64 fcmServiceAccount of
    Right bs -> Aeson.eitherDecode bs
    _ -> Left "FCM JSON file is not set in configs"

getNewToken :: (Redis.HedisFlow m r, MonadFlow m) => FCMConfig -> m (Either String JWT.JWToken)
getNewToken config = either (pure . Left) (refreshToken config) $ parseFCMAccount config.fcmServiceAccount

refreshToken :: (Redis.HedisFlow m r, MonadFlow m) => FCMConfig -> JWT.ServiceAccount -> m (Either String JWT.JWToken)
refreshToken config fcmAcc = do
  logTagInfo fcmTag "Refreshing token"
  refreshRes <- liftIO $ JWT.doRefreshToken fcmAcc
  case refreshRes of
    Left err -> do
      logTagInfo fcmTag $ fromString err
      pure $ Left $ fromString err
    Right token -> do
      logTagInfo fcmTag $ fromString "Success"
      Redis.set (config.fcmTokenKeyPrefix <> ":" <> redisFcmKey) token
      pure $ Right token
  where
    fcmTag = "FCM"
