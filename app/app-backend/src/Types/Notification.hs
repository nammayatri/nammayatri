{-# LANGUAGE DerivingStrategies #-}

module Types.Notification where

import Beckn.Utils.JSON
import EulerHS.Prelude
import Types.Storage.Case

data NotificationType
  = LEAD
  | CUSTOMER_APPROVED
  deriving (Generic, ToJSON)

instance FromJSON NotificationType where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

data Notification a = Notification
  { _type :: NotificationType,
    payload :: a
  }
  deriving (Generic, ToJSON)

instance FromJSON a => FromJSON (Notification a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

type CaseNotification = Notification Case
