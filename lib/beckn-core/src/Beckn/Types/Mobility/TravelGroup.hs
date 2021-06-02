module Beckn.Types.Mobility.TravelGroup where

import Beckn.Types.Core.Person
import Beckn.Utils.JSON
import EulerHS.Prelude

data TravelGroup = TravelGroup
  { primary_traveller :: Person, -- "PULL", "PUSH"
    group_size :: Int
  }
  deriving (Generic, Show)

instance FromJSON TravelGroup where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON TravelGroup where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
