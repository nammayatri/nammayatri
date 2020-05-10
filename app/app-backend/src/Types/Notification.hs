{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Notification where

import Beckn.Types.Storage.Person
import Data.Swagger
import Epass.Types.Common
import Epass.Types.Storage.RegistrationToken
import EulerHS.Prelude
import Servant.Swagger
import Beckn.Types.Storage.Case


data NotificationType
  = LEAD
  | CUSTOMER_APPROVED
  deriving (Generic, ToJSON)

instance FromJSON NotificationType where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data Notification a =
  Notification
    { _type    :: NotificationType
    , _payload :: a
    }
  deriving (Generic, ToJSON)

instance FromJSON a => FromJSON (Notification a) where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

type CaseNotification = Notification Case