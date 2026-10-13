{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Shared so both rider-app and driver-app use one definition. Separate module because five
-- of its constructors collide with TrackingStatus, and constructor names are the JSON values.
module Domain.Types.JourneyLegStatus where

import Data.OpenApi (ToParamSchema, ToSchema)
import EulerHS.Prelude

data JourneyLegStatus
  = InPlan
  | Assigning
  | Booked
  | AtRiskOfMissing
  | Missed
  | Delayed
  | Arriving
  | Arrived
  | OnTheWay
  | Ongoing
  | Skipped
  | Finishing
  | Cancelled
  | Completed
  | Failed
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)
