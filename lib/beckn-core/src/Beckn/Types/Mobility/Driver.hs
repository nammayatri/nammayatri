module Beckn.Types.Mobility.Driver where

import Beckn.Types.Core.Item
import Beckn.Types.Core.Person
import Data.Text
import EulerHS.Prelude

data Driver = Driver
  { _descriptor :: Person,
    _experience :: Experience
  }
  deriving (Generic, Show)

instance FromJSON Driver where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Driver where
  toJSON = genericToJSON stripLensPrefixOptions

data Experience = Experience
  { _label :: Text,
    _value :: Text,
    _unit :: Text
  }
  deriving (Generic, Show)

instance FromJSON Experience where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Experience where
  toJSON = genericToJSON stripLensPrefixOptions
