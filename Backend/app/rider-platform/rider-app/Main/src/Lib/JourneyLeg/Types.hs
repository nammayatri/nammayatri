module Lib.JourneyLeg.Types where

import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data JourneyLegStatus
  = InPlan
  | -- | Booking
    -- | RetryBooking
    Assigning
  | -- | ReAssigning
    Booked
  | OnTime
  | AtRiskOfMissing
  | Departed
  | Missed
  | Delayed
  | Arriving
  | Arrived
  | OnTheWay
  | Skipped -- we might need this
  | Ongoing
  | Finishing
  | Cancelled
  | Completed
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''JourneyLegStatus)

data JourneySearchData = JourneySearchData
  { journeyId :: Text,
    journeyLegOrder :: Int,
    agency :: Maybe Text,
    skipBooking :: Bool,
    convenienceCost :: Int,
    pricingId :: Maybe Text,
    isDeleted :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)
