module Beckn.Types.Mobility.TravelGroup where

import Beckn.Types.Core.Person
import EulerHS.Prelude

data TravelGroup = TravelGroup
  { primary_traveller :: Person, -- "PULL", "PUSH"
    group_size :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)
