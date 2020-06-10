{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Beckn.External.FCM.Flow where

import Beckn.External.FCM.Types
import Beckn.Types.Storage.Person as Person
import qualified Beckn.Utils.JWT as JWT
import Control.Lens
import Data.Bool
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
notifyPerson :: Text -> Text -> FCMData -> Person -> L.Flow ()
notifyPerson title body msgData person =
  let pid = show (Person._id person)
      tokenNotFound = "device token of a person " <> show pid <> " not found"
   in case Person._deviceToken person of
        Nothing -> do
          L.logInfo (T.pack "FCM") tokenNotFound
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

defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "fcm.googleapis.com",
      baseUrlPort = 443,
      baseUrlPath = "/v1/projects/jp-beckn-dev/messages:send"
    }

-- | Send FCM message to a registered device
sendMessage :: FCMRequest -> String -> L.Flow ()
sendMessage fcmMsg toWhom = L.forkFlow desc $ do
  authToken <- getToken
  case authToken of
    Right token -> do
      res <- L.callAPI defaultBaseUrl $ callFCM (Just $ FCMAuthToken token) fcmMsg
      L.logInfo (T.pack "FCM") $ case res of
        Right _ -> "message sent successfully to " <> T.pack toWhom
        Left x -> "error: " <> show x
      pure ()
    Left err -> do
      L.logInfo (T.pack "FCM") $ "error: " <> show err
      pure ()
  where
    callFCM token msg = void $ ET.client fcmSendMessageAPI token msg
    desc = "FCM send message forked flow"

-- | try to get FCM text token
getToken :: L.Flow (Either String Text)
getToken = do
  token <- checkToken
  pure $ case token of
    Left err -> Left err
    Right t -> Right $ JWT._jwtTokenType t <> T.pack " " <> JWT._jwtAccessToken t

checkToken :: L.Flow (Either String FCMToken)
checkToken = checkAndGetToken True

checkAndRefreshToken :: L.Flow (Either String FCMToken)
checkAndRefreshToken = checkAndGetToken False

-- | check FCM token and refresh if it is invalid
checkAndGetToken :: Bool -> L.Flow (Either String FCMToken)
checkAndGetToken readOnly = do
  token <- L.getOption FCMTokenKey
  case token of
    Nothing ->
      -- token not found
      if readOnly
        then pure $ Left "Token not found"
        else refreshToken
    Just t -> do
      validityStatus <- L.runIO $ JWT.isValid t
      if readOnly
        then pure $ case validityStatus of
          JWT.JWTValid _ -> Right t
          JWT.JWTExpired _ -> Left "Token expired"
          JWT.JWTInvalid -> Left "Token is invalid"
        else case validityStatus of
          JWT.JWTValid x ->
            if x < 300
              then pure $ Right t -- do nothing, token is ok
              else do
                -- close to expiration, start trying to refresh token
                L.logInfo fcm "Token is about to be expiried, trying to refresh it"
                refreshToken
          JWT.JWTExpired x -> do
            -- token expired
            L.logInfo fcm $ "Token expired " <> show x <> " seconds ago, trying to refresh it"
            L.delOption FCMTokenKey
            refreshToken
          JWT.JWTInvalid -> do
            -- token is invalid
            L.logInfo fcm "Token is invalid, trying to refresh it"
            L.delOption FCMTokenKey
            refreshToken
  where
    fcm = T.pack "FCM"
    refreshToken = do
      L.logInfo fcm "Refreshing token"
      t <- L.runIO JWT.refreshToken
      case t of
        Left err -> do
          L.logInfo fcm $ T.pack err
          pure $ Left err
        Right token -> do
          L.logInfo fcm $ T.pack "Success"
          L.setOption FCMTokenKey token
          pure $ Right token
