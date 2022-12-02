module Product.Fcm where

import App.Types
import Beckn.External.FCM.Types
import Beckn.Types.Error
import Beckn.Utils.Error
import Beckn.Utils.Logging
import Beckn.Utils.Text
import Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import Data.Aeson
import qualified Data.Map as Map
import EulerHS.Prelude
import Types.API.Fcm

sendFcm ::
  Maybe FCMAuthToken ->
  FCMRequest Value ->
  FlowHandler FCMResponse
sendFcm _authToken (FCMRequest ntf) = withFlowHandler $ do
  to <- ntf.fcmToken & fromMaybeM (InvalidRequest "No token")
  logPretty INFO ("Message for " <> encodeToText to) ntf
  asks notificationsMap >>= liftIO . (`modifyMVar_` (pure . set to))
  return $ FCMResponse Nothing Nothing
  where
    set to ntfs = Map.insert to (ntf : fromMaybe [] (Map.lookup to ntfs)) ntfs

readFcm :: FCMRecipientToken -> FlowHandler ReadFcmRes
readFcm number = withFlowHandler $ do
  asks notificationsMap >>= liftIO . (`modifyMVar` (pure . take'))
  where
    take' ntfs = Map.lookup number ntfs & maybe (ntfs, []) (Map.delete number ntfs,)
