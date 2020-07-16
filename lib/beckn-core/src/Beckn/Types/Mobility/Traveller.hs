module Beckn.Types.Mobility.Traveller where

import Beckn.Types.Core.Person
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Traveller = Traveller
  { _info :: Person,
    _origin_stop_id :: Text,
    _destination_stop_id :: Text
  }
  deriving (Generic, Show)

instance FromJSON Traveller where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Traveller where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Traveller where
  example =
    Traveller
      { _info = example,
        _origin_stop_id = idExample,
        _destination_stop_id = idExample
      }
