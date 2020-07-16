module Beckn.Types.Core.Operator where

import Beckn.Types.Core.Person
import Beckn.Utils.Common
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

instance Example Operator where
  example =
    Operator
      { _descriptor = example,
        _experience = example
      }

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

instance Example Experience where
  example =
    Experience
      { _label = "Senior",
        _value = "8",
        _unit = "years"
      }
