--{-# LANGUAGE OverloadedStrings #-}

module Beckn.External.FCM.Flow where

import           Beckn.External.FCM.Types
import           Beckn.Types.Storage.Person as Person
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8      as UTF8
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import qualified EulerHS.Language          as L
import           EulerHS.Prelude
import           FCMClient
import           FCMClient.Types
import           System.Environment

-- | Create FCM message
-- Note that data should be formed as key-value pairs list
-- recipientId::FCMToken is an app's registration token
createMessage :: ToJSON a => a -> FCMNotification -> FCMRecipientToken -> FCMMessage
createMessage msgData notification recipientId =
  def & fcmTo ?~ getFCMRecipientToken recipientId
      & fcmData ?~ Map.fromList [("jsondata", (T.pack . LUTF8.toString . encode) msgData)]
      & fcmNotification ?~ notification

-- | Notification details
createNotification :: Text -> Text -> FCMNotification
createNotification title body =
  def & fcmTitle ?~ title
      & fcmBody ?~ body

-- | Send FCM message to a registered device
sendMessageIO :: FCMMessage -> IO FCMResult
sendMessageIO msg = do
  cliAuthKey <- UTF8.fromString <$> getEnv "FCM_AUTH_TOKEN"
  fcmCallJSON cliAuthKey msg

-- | Send FCM message to a registered device
sendMessage :: FCMMessage -> L.Flow (Either Text FCMResponseBody)
sendMessage msg = do
  res <- L.runIO $ do
    res <- sendMessageIO msg
    pure $ case res of
      FCMResultSuccess b -> Right b
      FCMResultError e -> Left $ case e of
        FCMErrorResponseInvalidAuth -> T.pack "Error authenticating the sender account"
        FCMServerError _ m -> m
        FCMErrorResponseInvalidJSON m -> m
        FCMClientJSONError m -> m
        FCMClientHTTPError m -> m
  L.logInfo (T.pack "FCM") $ case res of
    Left m -> "Error: " <> m
    _      -> "Success"
  pure res

-- | Send FCM message to a person
notifyPerson :: ToJSON a => Text -> Text -> a -> Person -> L.Flow (Either Text FCMResponseBody)
notifyPerson title body msgData person =
  case Person._deviceToken person of
    Nothing -> pure $ Left $ "device token of a person " <> show (Person._id person) <> "not found"
    Just token ->
      sendMessage $
        createMessage msgData (createNotification title body) token