module Lib.JourneyLeg.Types where

import Domain.Types.Route
import Domain.Types.Station
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
    onSearchFailed :: Maybe Bool,
    isDeleted :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data MultiModalJourneyRouteDetails = MultiModalJourneyRouteDetails
  { frequency :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    alternateShortNames :: [Kernel.Prelude.Text],
    lineColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lineColorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    journeyStatus :: Kernel.Prelude.Maybe JourneyLegStatus,
    subLegOrder :: Kernel.Prelude.Maybe Int,
    platformNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeLongName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Route.Route),
    fromStationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Station.Station),
    toStationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Station.Station)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CrisSearchData = CrisSearchData
  { crisSdkToken :: Maybe Text,
    bookAuthCode :: Maybe Text,
    deviceId :: Maybe Text,
    osBuildVersion :: Maybe Text,
    osType :: Maybe Text,
    distance :: Maybe Int,
    trainType :: Maybe Text,
    crisAppSession :: Maybe Int,
    via :: Maybe Text,
    crisRouteId :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)
