module Lib.JourneyPlannerTypes where

import Kernel.Prelude

data JourneySearchData = JourneySearchData
  { journeyId :: Text,
    journeyLegOrder :: Int,
    agency :: Maybe Text,
    skipBooking :: Bool,
    convenienceCost :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
