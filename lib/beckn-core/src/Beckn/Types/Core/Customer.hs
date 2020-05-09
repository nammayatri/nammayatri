module Beckn.Types.Core.Customer where

import Beckn.Types.Core.Person
import Data.Text
import EulerHS.Prelude

data Customer = Customer
  { _type :: Text, --"SINGLE", "GROUP"
    _single :: (Maybe Person),
    _group :: (Maybe GroupCustomer)
  }
  deriving (Generic, Show)

instance FromJSON Customer where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Customer where
  toJSON = genericToJSON stripAllLensPrefixOptions

data GroupCustomer = GroupCustomer
  { _primary :: Person,
    _count :: Integer
  }
  deriving (Generic, Show)

instance FromJSON GroupCustomer where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON GroupCustomer where
  toJSON = genericToJSON stripAllLensPrefixOptions
