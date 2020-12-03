module Beckn.Types.Core.Migration.Billing
  ( Billing (..),
  )
where

import Beckn.Types.Core.Migration.Address (Address)
import Beckn.Types.Core.Migration.Organization (Organization)
import Beckn.Types.Core.Migration.Time (Time)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Billing = Billing
  { _name :: Text,
    _organization :: Maybe Organization,
    _address :: Maybe Address,
    _email :: Maybe Text,
    _phone :: Text,
    _time :: Maybe Time,
    _tax_number :: Maybe Text,
    _created_at :: Maybe UTCTime,
    _updated_at :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Billing where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Billing where
  toJSON = genericToJSON stripLensPrefixOptions
