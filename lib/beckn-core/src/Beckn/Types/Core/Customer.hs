module Beckn.Types.Core.Customer where

import Beckn.Types.Core.Person
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Customer = Customer
  { _type :: Text, --"SINGLE", "GROUP"
    single :: Maybe Person,
    group :: Maybe GroupCustomer
  }
  deriving (Generic, Show)

instance FromJSON Customer where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Customer where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data GroupCustomer = GroupCustomer
  { primary :: Person,
    count :: Integer
  }
  deriving (Generic, Show)

instance FromJSON GroupCustomer where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON GroupCustomer where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
