{-# LANGUAGE DerivingStrategies #-}

module Types.Notification where

import Beckn.Types.Storage.Case
import EulerHS.Prelude

data NotificationType
  = LEAD
  | CUSTOMER_APPROVED
  deriving (Generic, ToJSON)

instance FromJSON NotificationType where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data Notification a = Notification
  { _type :: NotificationType,
    _payload :: a
  }
  deriving (Generic, ToJSON)

instance FromJSON a => FromJSON (Notification a) where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

type CaseNotification = Notification Case
