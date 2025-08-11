module Lib.JourneyModule.State.Types where

import Data.Aeson
import qualified Domain.Types.BookingStatus as DTaxiBooking
import qualified Domain.Types.EstimateStatus as DTaxiEstimate
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.RideStatus as DTaxiRide
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data JourneyBookingStatus
  = TaxiEstimate DTaxiEstimate.EstimateStatus
  | TaxiBooking DTaxiBooking.BookingStatus
  | TaxiRide DTaxiRide.RideStatus
  | FRFSBooking DFRFSBooking.FRFSTicketBookingStatus
  | FRFSTicket DFRFSTicket.FRFSTicketStatus
  | Feedback FeedbackStatus
  deriving (Generic, ToSchema, ToJSON, FromJSON, Show, Eq, Ord)

data FeedbackStatus = FEEDBACK_PENDING
  deriving (Generic, ToSchema, Show, Eq, Ord)

instance FromJSON FeedbackStatus where
  parseJSON _ = return FEEDBACK_PENDING

instance ToJSON FeedbackStatus where
  toJSON FEEDBACK_PENDING = "FEEDBACK_PENDING"

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

$(mkHttpInstancesForEnum ''TrackingStatus)
$(mkBeamInstancesForEnum ''TrackingStatus)
