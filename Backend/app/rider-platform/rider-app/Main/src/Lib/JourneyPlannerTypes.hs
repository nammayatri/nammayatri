module Lib.JourneyPlannerTypes where

import Kernel.Prelude

data JourneySearchData = JourneySearchData
  { journeyId :: Text,
    journeyLegOrder :: Int,
    agency :: Maybe Text,
    skipBooking :: Bool,
    convenienceCost :: Int,
    pricingId :: Maybe Text,
    isSkipped :: Bool,
    isCancelled :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)
