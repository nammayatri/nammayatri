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
import Beckn.Types.Common (FlowR)
import Beckn.Types.Id
import Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common (fork)
import qualified Beckn.Utils.JWT as JWT
import Beckn.Utils.Logging (HasLogContext, Log (..))
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
  ( HasLogContext r,
    HasField "fcmUrl" r BaseUrl,
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
          logInfo "FCM" tokenNotFound
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
  ( HasLogContext r,
    HasField "fcmUrl" r BaseUrl,
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
      logInfo fcm $ case res of
        Right _ -> "message sent successfully to a person with id = " <> toWhom
        Left x -> "error: " <> show x
      pure ()
    Left err -> do
      logError fcm $ "error: " <> show err
      pure ()
  where
    callFCM token msg = void $ ET.client fcmSendMessageAPI token msg
    desc = "FCM send message forked flow"
    fcm = "FCM"

-- | try to get FCM text token
getTokenText ::
  ( HasField "fcmJsonPath" r (Maybe Text),
    HasLogContext r
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
checkAndGetToken sa = do
  token <- L.getOption FCMTokenKey
  case token of
    Nothing -> refreshToken
    Just t -> do
      validityStatus <- L.runIO $ JWT.isValid t
      case validityStatus of
        JWT.JWTValid x ->
          if x > 300
            then pure $ Right t -- do nothing, token is ok
            else do
              -- close to expiration, start trying to refresh token
              logInfo fcm "Token is about to be expired, trying to refresh it"
              refreshToken
        JWT.JWTExpired x -> do
          -- token expired
          logInfo fcm $ "Token expired " <> show x <> " seconds ago, trying to refresh it"
          L.delOption FCMTokenKey
          refreshToken
        JWT.JWTInvalid -> do
          -- token is invalid
          logInfo fcm "Token is invalid, trying to refresh it"
          L.delOption FCMTokenKey
          refreshToken
  where
    fcm = "FCM"
    refreshToken = do
      logInfo fcm "Refreshing token"
      t <- L.runIO $ JWT.doRefreshToken sa
      case t of
        Left err -> do
          logError fcm $ fromString err
          pure $ Left $ fromString err
        Right token -> do
          logInfo fcm $ fromString "Success"
          L.setOption FCMTokenKey token
          pure $ Right token

-- | Get token (refresh token if expired / invalid)
getToken ::
  ( HasField "fcmJsonPath" r (Maybe Text),
    HasLogContext r
  ) =>
  FlowR r (Either String JWT.JWToken)
getToken = do
  tokenStatus <-
    L.getOption FCMTokenKey >>= \case
      Nothing -> pure $ Left "Token not found"
      Just jwt -> do
        validityStatus <- L.runIO $ JWT.isValid jwt
        pure $ case validityStatus of
          JWT.JWTValid _ -> Right jwt
          JWT.JWTExpired _ -> Left "Token expired"
          JWT.JWTInvalid -> Left "Token is invalid"
  case tokenStatus of
    Left err -> do
      logWarning "FCM" $ "Refreshing FCM token. Reason: " <> fromString err
      getAndParseFCMAccount >>= either (pure . Left) checkAndGetToken
    jwt -> pure jwt

getAndParseFCMAccount ::
  HasField "fcmJsonPath" r (Maybe Text) =>
  FlowR r (Either String JWT.ServiceAccount)
getAndParseFCMAccount = do
  mbFcmFile <- getField @"fcmJsonPath" <$> ask
  rawContent <- L.runIO $ E.try @SomeException (BL.readFile . toString $ fromMaybe "" mbFcmFile)
  pure $ parseContent $ first show rawContent
  where
    parseContent :: Either String BL.ByteString -> Either String JWT.ServiceAccount
    parseContent rawContent = rawContent >>= Aeson.eitherDecode
