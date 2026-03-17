{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.CommunicationEngine.Types
  ( CommunicationDirective (..),
    CommunicationAction (..),
    FcmNotificationParams (..),
    InAppOverlayParams (..),
    InAppMessageParams (..),
    SmsParams (..),
    BadgeParams (..),
    CommunicationHandler (..),
    CommunicationResult (..),
  )
where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Types.App ()

-- orphan ToSchema Value instance

-- | Input directive — can come from behavior-engine, dashboard, or any app flow
data CommunicationDirective = CommunicationDirective
  { channel :: Text, -- "FCM_NOTIFICATION", "IN_APP_OVERLAY", "SMS", etc.
    templateKey :: Text, -- template identifier for the message
    params :: Value, -- template variables
    delaySeconds :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parsed, typed communication action from rule engine output
data CommunicationAction
  = FcmNotification FcmNotificationParams
  | InAppOverlay InAppOverlayParams
  | InAppMessage InAppMessageParams
  | SmsCommunication SmsParams
  | BadgeCommunication BadgeParams
  | NoCommunication
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for FCM push notification
data FcmNotificationParams = FcmNotificationParams
  { templateKey :: Text, -- notification template identifier
    templateParams :: Value -- template variables (cancellationRate, etc.)
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for in-app overlay/popup
data InAppOverlayParams = InAppOverlayParams
  { overlayKey :: Text, -- overlay template identifier (e.g. "CANCELLATION_RATE_BLOCK_WEEKLY")
    templateParams :: Value, -- template variables
    showCloseButton :: Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for persistent in-app message
data InAppMessageParams = InAppMessageParams
  { messageKey :: Text,
    templateParams :: Value,
    expiryHours :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for SMS
data SmsParams = SmsParams
  { templateKey :: Text,
    templateParams :: Value
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for badge on profile
data BadgeParams = BadgeParams
  { badgeKey :: Text,
    badgeType :: Text, -- "WARNING", "INFO", etc.
    expiryHours :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Result of dispatching a communication
data CommunicationResult = CommunicationResult
  { action :: CommunicationAction,
    success :: Bool,
    errorMessage :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Typeclass for apps to implement communication dispatch.
--
-- The @entityId@ parameter is the entity (driver/rider) to communicate with.
--
-- Apps provide instances that delegate to their domain-specific functions, e.g.:
--   sendFcmNotification → Notify.sendCancellationRateNudgeOverlay (driver app)
--   sendFcmNotification → Notify.sendCustomerCancellationRateNudge (rider app)
class (Monad m) => CommunicationHandler m where
  sendFcmNotification :: Text -> FcmNotificationParams -> m ()
  sendInAppOverlay :: Text -> InAppOverlayParams -> m ()
  sendInAppMessage :: Text -> InAppMessageParams -> m ()
  sendSms :: Text -> SmsParams -> m ()
  sendBadge :: Text -> BadgeParams -> m ()

  -- | Optional: called when communication is NoCommunication. Default: no-op.
  handleNoCommunication :: Text -> m ()
  handleNoCommunication _ = pure ()

  -- | Optional: log/record the communication for audit. Default: no-op.
  recordCommunication :: Text -> CommunicationAction -> Value -> m ()
  recordCommunication _ _ _ = pure ()
