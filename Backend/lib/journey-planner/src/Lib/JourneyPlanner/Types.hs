module Lib.JourneyPlanner.Types where

import Kernel.Prelude
import Kernel.Types.Id

data Journey

data JourneySearchData = JourneySearchData
  { journeyId :: Id Journey,
    journeyLegOrder :: Int,
    agency :: Maybe Text,
    skipBooking :: Bool,
    convenienceCost :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
