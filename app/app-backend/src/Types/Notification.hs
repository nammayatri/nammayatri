{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Notification where

import Beckn.Types.Common
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Person
import Beckn.Types.Storage.RegistrationToken
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

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
