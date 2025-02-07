module Lib.JourneyLeg.Types where

import Domain.Types.Station
-- import qualified Kernel.External.Maps.Google.MapsClient.Types as GT
-- import Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
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

data MultiModalJourneyRouteDetails = MultiModalJourneyRouteDetails
  { frequency :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    lineColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lineColorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    platformNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeLongName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Station.Station),
    toStationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Station.Station)
    -- fromArrivalTime :: Maybe UTCTime,
    -- fromDepartureTime :: Maybe UTCTime,
    -- toArrivalTime :: Maybe UTCTime,
    -- toDepartureTime :: Maybe UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
