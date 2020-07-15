module Beckn.Types.Mobility.Traveller where

import Beckn.Types.Core.Person
import Beckn.Types.Core.Rating
import Beckn.Types.Mobility.Stop
import Beckn.Utils.Common
import Data.Text
import Data.Time
import EulerHS.Prelude

data Traveller = Traveller
  { _descriptor :: Person,
    _rating :: Rating,
    _origin :: Stop,
    _destination :: Stop
  }
  deriving (Generic, Show)

instance FromJSON Traveller where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Traveller where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Traveller where
  example =
    Traveller
      { _descriptor = example,
        _rating = example,
        _origin = example,
        _destination = example
      }
