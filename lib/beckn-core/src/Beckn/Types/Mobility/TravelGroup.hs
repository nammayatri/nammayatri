module Beckn.Types.Mobility.TravelGroup where

import Beckn.Types.Core.Person
import EulerHS.Prelude

data TravelGroup = TravelGroup
  { _primary_traveller :: Person, -- "PULL", "PUSH"
    _group_size :: Int
  }
  deriving (Generic, Show)

instance FromJSON TravelGroup where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON TravelGroup where
  toJSON = genericToJSON stripLensPrefixOptions
