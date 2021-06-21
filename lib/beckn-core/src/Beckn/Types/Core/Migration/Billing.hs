module Beckn.Types.Core.Migration.Billing
  ( Billing (..),
  )
where

import Beckn.Types.Core.Migration.Address (Address)
import Beckn.Types.Core.Migration.Organization (Organization)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Time (UTCTime)
import EulerHS.Prelude

data Billing = Billing
  { name :: Text,
    organization :: Maybe Organization,
    address :: Maybe Address,
    email :: Maybe Text,
    phone :: Text,
    time :: Maybe Time,
    tax_number :: Maybe Text,
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Billing where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Billing where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Billing where
  example =
    Billing
      { name = "Mr. Payeerson",
        organization = Nothing,
        address = Nothing,
        email = Nothing,
        phone = "+919999999999",
        time = Nothing,
        tax_number = Nothing,
        created_at = Nothing,
        updated_at = Nothing
      }
