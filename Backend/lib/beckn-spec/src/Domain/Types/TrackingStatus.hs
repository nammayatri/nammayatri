{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Shared so both rider-app and driver-app use one definition.
module Domain.Types.TrackingStatus where

import Data.OpenApi (ToParamSchema, ToSchema)
import EulerHS.Prelude

data TrackingStatus
  = InPlan
  | -- Realtime Vehicle Approaching Pickup Location (Backend)
    Arriving -- (Taxi - Backend OnUpdate)
  | AlmostArrived -- (Taxi - Backend OnUpdate)
  | Arrived -- (Taxi - Backend OnUpdate)
  -- Post Ride Start Status
  | Ongoing -- (Taxi - Ride NEW)
  | Finishing
  | ExitingStation -- (FRFS - At Destination Station)
  -- Terminal Status
  | Finished -- (Taxi - Ride COMPLETED)
  deriving (Generic, ToSchema, Eq, Ord, Show, Read, FromJSON, ToJSON, ToParamSchema)
