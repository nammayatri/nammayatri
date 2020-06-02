module Utils.FCM where

import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Types.App
import Types.Notification

sendNotification :: (Notification a) -> Text -> L.Flow ()
sendNotification notif deviceToken = undefined
