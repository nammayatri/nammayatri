module Beckn.Types.Core.Operator where

import Beckn.Types.Core.Person
import Data.Text
import EulerHS.Prelude

data Operator = Operator
  { _descriptor :: Person,
    _experience :: Experience
  }
  deriving (Generic, Show)

instance FromJSON Operator where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Operator where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Experience = Experience
  { _label :: Text,
    _value :: Text,
    _unit :: Text
  }
  deriving (Generic, Show)

instance FromJSON Experience where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Experience where
  toJSON = genericToJSON stripAllLensPrefixOptions
