module Beckn.Types.FMD.Agent where

import Beckn.Types.Core.Operator (Experience)
import Beckn.Types.Core.Person
import Beckn.Utils.Common
import EulerHS.Prelude

data Agent = Agent
  { _descriptor :: Person,
    _experience :: Experience
  }
  deriving (Generic, Show)

instance FromJSON Agent where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Agent where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Agent where
  example =
    Agent
      { _descriptor = example,
        _experience = example
      }
