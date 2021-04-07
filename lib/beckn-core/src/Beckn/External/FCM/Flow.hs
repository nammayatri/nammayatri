{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import Beckn.Types.Id
import Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common
import qualified Beckn.Utils.JWT as JWT
import Control.Exception (IOException)
import qualified Control.Exception as E (try)
import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding ((^.))
import qualified EulerHS.Types as ET
import GHC.Records (HasField (..))
import Servant
import Servant.Client

-- | Create FCM message
-- Note that data should be formed as key-value pairs list
-- recipientId::FCMToken is an app's registration token
createMessage :: FCMNotificationTitle -> FCMNotificationBody -> FCMData -> FCMRecipientToken -> FCMMessage
createMessage title body msgData recipientId =
  def & fcmToken ?~ recipientId
    & fcmData ?~ msgData
    & fcmAndroid ?~ androidCfg
  where
    tag = msgData ^. fcmNotificationType
    androidCfg = createAndroidConfig title body tag

-- | Android Notification details
createAndroidConfig :: FCMNotificationTitle -> FCMNotificationBody -> FCMNotificationType -> FCMAndroidConfig
createAndroidConfig title body tag =
  def & fcmdNotification ?~ notification
  where
    notification = createAndroidNotification title body tag

createAndroidNotification :: FCMNotificationTitle -> FCMNotificationBody -> FCMNotificationType -> FCMAndroidNotification
createAndroidNotification title body notificationType =
  let notification = case notificationType of
        ALLOCATION_REQUEST ->
          def & fcmdSound ?~ "notify_sound.mp3"
            & fcmdChannelId ?~ "RINGING_ALERT"
        TRIP_STARTED ->
          def & fcmdSound ?~ "notify_otp_sound.mp3"
            & fcmdChannelId ?~ "TRIP_STARTED"
        _ -> def
   in notification & fcmdTitle ?~ title
        & fcmdBody ?~ body
        & fcmdIcon
          ?~ FCMNotificationIconUrl
            "https://api.sandbox.beckn.juspay.in/static/images/ride-success.png"
        & fcmdTag ?~ notificationType

-- | Send FCM message to a person
notifyPerson ::
  ( HasField "fcmUrl" r BaseUrl,
    HasField "fcmJsonPath" r (Maybe Text)
  ) =>
  FCMNotificationTitle ->
  FCMNotificationBody ->
  FCMData ->
  Person ->
  FlowR r ()
notifyPerson title body msgData person =
  let pid = getId $ Person._id person
      tokenNotFound = "device token of a person " <> pid <> " not found"
   in case Person._deviceToken person of
        Nothing -> do
          logTagInfo "FCM" tokenNotFound
          pure ()
        Just token ->
          sendMessage (FCMRequest (createMessage title body msgData token)) pid

-- | Google API interface
type FCMSendMessageAPI =
  Header "Authorization" FCMAuthToken
    :> ReqBody '[JSON] FCMRequest
    :> Post '[JSON] FCMResponse

fcmSendMessageAPI :: Proxy FCMSendMessageAPI
fcmSendMessageAPI = Proxy

-- | Send FCM message to a registered device
sendMessage ::
  ( HasField "fcmUrl" r BaseUrl,
    HasField "fcmJsonPath" r (Maybe Text)
  ) =>
  FCMRequest ->
  Text ->
  FlowR r ()
sendMessage fcmMsg toWhom = fork desc $ do
  authToken <- getTokenText
  case authToken of
    Right token -> do
      fcmUrl <- getField @"fcmUrl" <$> ask
      res <- L.callAPI fcmUrl $ callFCM (Just $ FCMAuthToken token) fcmMsg
      logTagInfo fcm $ case res of
        Right _ -> "message sent successfully to a person with id = " <> toWhom
        Left x -> "error: " <> show x
      pure ()
    Left err -> do
      logTagError fcm $ "error: " <> show err
      pure ()
  where
    callFCM token msg = void $ ET.client fcmSendMessageAPI token msg
    desc = "FCM send message forked flow"
    fcm = "FCM"

-- | try to get FCM text token
getTokenText ::
  ( HasField "fcmJsonPath" r (Maybe Text)
  ) =>
  FlowR r (Either Text Text)
getTokenText = do
  token <- getToken
  pure $ case token of
    Left err -> Left $ fromString err
    Right t -> Right $ JWT._jwtTokenType t <> " " <> JWT._jwtAccessToken t

-- | check FCM token and refresh if it is invalid
checkAndGetToken ::
  (L.MonadFlow m, Log m) =>
  JWT.ServiceAccount ->
  m (Either String JWT.JWToken)
checkAndGetToken fcmAcc = do
  token <- Redis.getKeyRedis "beckn:fcm_token"
  case token of
    Nothing -> refreshToken fcmAcc
    Just t -> do
      validityStatus <- L.runIO $ JWT.isValid t
      case validityStatus of
        JWT.JWTValid x ->
          if x > 300
            then pure $ Right t -- do nothing, token is ok
            else do
              -- close to expiration, start trying to refresh token
              logTagInfo fcm "Token is about to be expired, trying to refresh it"
              refreshToken fcmAcc
        JWT.JWTExpired x -> do
          -- token expired
          logTagInfo fcm $ "Token expired " <> show x <> " seconds ago, trying to refresh it"
          refreshToken fcmAcc
        JWT.JWTInvalid -> do
          -- token is invalid
          logTagInfo fcm "Token is invalid, trying to refresh it"
          refreshToken fcmAcc
  where
    fcm = "FCM"

-- | Get token (refresh token if expired / invalid)
getToken ::
  ( HasField "fcmJsonPath" r (Maybe Text)
  ) =>
  FlowR r (Either String JWT.JWToken)
getToken = do
  tokenStatus <-
    Redis.getKeyRedis "beckn:fcm_token" >>= \case
      Nothing -> pure $ Left "Token not found"
      Just jwt -> do
        validityStatus <- L.runIO $ JWT.isValid jwt
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
  HasField "fcmJsonPath" r (Maybe Text) =>
  FlowR r (Either String JWT.ServiceAccount)
getAndParseFCMAccount = do
  mbFcmFile <- getField @"fcmJsonPath" <$> ask
  case mbFcmFile of
    Nothing -> pure $ Left "FCM JSON file is not set in configs"
    Just fcmFile -> do
      rawContent <- L.runIO . E.try @IOException . BL.readFile $ toString fcmFile
      pure $ parseContent $ first show rawContent
  where
    parseContent :: Either String BL.ByteString -> Either String JWT.ServiceAccount
    parseContent rawContent = rawContent >>= Aeson.eitherDecode

getNewToken :: (HasField "fcmJsonPath" r (Maybe Text)) => FlowR r (Either String JWT.JWToken)
getNewToken = getAndParseFCMAccount >>= either (pure . Left) refreshToken

refreshToken :: (L.MonadFlow m, Log m) => JWT.ServiceAccount -> m (Either String JWT.JWToken)
refreshToken fcmAcc = do
  logTagInfo fcmTag "Refreshing token"
  refreshRes <- L.runIO $ JWT.doRefreshToken fcmAcc
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
