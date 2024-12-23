module Lib.JourneyLeg.Types where

import Kernel.Prelude

data JourneyLegState = {
  status :: JourneyLegStatus,
  currentPosition :: LatLong | Station
}

data JourneyLeg = JourneyLeg
  { distance :: Distance.Distance,
    journeyId :: Id Journey,
    duration :: Time.Seconds,
    polyline :: GT.Polyline,
    mode :: GeneralVehicleType,
    startLocation :: GT.LocationV2,
    endLocation :: GT.LocationV2,
    fromStopDetails :: Maybe MultiModalStopDetails,
    toStopDetails :: Maybe MultiModalStopDetails,
    routeDetails :: Maybe MultiModalRouteDetails,
    agency :: Maybe MultiModalAgency,
    fromArrivalTime :: Maybe UTCTime,
    fromDepartureTime :: Maybe UTCTime,
    toArrivalTime :: Maybe UTCTime,
    toDepartureTime :: Maybe UTCTime
    status : JourneyLegStatus
    sequenceNumber : Int
  }

type SearchJourneyLeg leg m = leg -> m ()
type ConfirmJourneyLeg leg m = leg -> m ()
type CancelJourneyLeg leg m = leg -> m ()
type UpdateJourneyLeg leg m = leg -> m ()
type GetJourneyLegState leg m = leg -> m JourneyLegState
type GetJourneyLeg leg m = leg -> m LegInfo

class JourneyLeg leg m where
  search :: SearchJourneyLeg leg m
  confirm :: ConfirmJourneyLeg leg m
  update :: UpdateJourneyLeg leg m
  cancel :: CancelJourneyLeg leg m
  getState :: GetJourneyLegState leg m
  get :: GetJourneyLeg leg m

data JourneyLegStatus =
    InPlan
  -- | Booking
  -- | RetryBooking
  | Assigning
  -- | ReAssigning
  | Booked
  | OnTime
  | AtRiskOfMissing
  | Departed
  | Missed
  | Delayed
  | Arriving
  | Skipped -- we might need this
  | Ongoing
  | Finishing
  | Cancelled
  | Completed
  deriving (Eq, Show)
