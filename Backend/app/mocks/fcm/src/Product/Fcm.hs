{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Product.Fcm where

import App.Types
import Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import Data.Aeson
import qualified Data.Map as Map
import EulerHS.Prelude
import Kernel.External.Notification.FCM.Types
import Kernel.Types.Error
import Kernel.Utils.Error
import Kernel.Utils.Logging
import Kernel.Utils.Text
import Types.API.Fcm

sendFcm ::
  Maybe FCMAuthToken ->
  FCMRequest Value ->
  FlowHandler FCMResponse
sendFcm _authToken (FCMRequest ntf) = withFlowHandler' $ do
  to <- ntf.fcmToken & fromMaybeM (InvalidRequest "No token")
  logPretty INFO ("Message for " <> encodeToText to) ntf
  asks notificationsMap >>= liftIO . (`modifyMVar_` (pure . set to))
  return $ FCMResponse Nothing Nothing
  where
    set to ntfs = Map.insert to (ntf : fromMaybe [] (Map.lookup to ntfs)) ntfs

readFcm :: FCMRecipientToken -> FlowHandler ReadFcmRes
readFcm number = withFlowHandler' $ do
  asks notificationsMap >>= liftIO . (`modifyMVar` (pure . take'))
  where
    take' ntfs = Map.lookup number ntfs & maybe (ntfs, []) (Map.delete number ntfs,)
