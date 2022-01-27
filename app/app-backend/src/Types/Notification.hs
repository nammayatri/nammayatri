{-# LANGUAGE DerivingStrategies #-}

module Types.Notification where

import Beckn.Utils.JSON
import Domain.Types.SearchRequest
import EulerHS.Prelude

data NotificationType
  = LEAD
  | CUSTOMER_APPROVED
  deriving (Generic, FromJSON, ToJSON)

data Notification a = Notification
  { _type :: NotificationType,
    payload :: a
  }
  deriving (Generic)

instance FromJSON a => FromJSON (Notification a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON a => ToJSON (Notification a) where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type SearchRequestNotification = Notification SearchRequest
