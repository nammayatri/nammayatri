module Lib.JourneyLeg.Types where

import Kernel.Prelude
import Kernel.Types.Common
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import qualified Tools.Beam.UtilsTH

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

instance FromHttpApiData JourneyLegStatus where
  parseQueryParam = readEither

instance ToHttpApiData JourneyLegStatus where
  toUrlPiece = show

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''JourneyLegStatus)

data MultiModalJourneyRouteDetails = MultiModalJourneyRouteDetails
  { frequency :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    alternateShortNames :: [Kernel.Prelude.Text],
    lineColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lineColorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    journeyStatus :: Kernel.Prelude.Maybe JourneyLegStatus,
    subLegOrder :: Kernel.Prelude.Maybe Int,
    platformNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeLongName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Maybe Text,
    fromStationCode :: Kernel.Prelude.Maybe Text,
    toStationCode :: Kernel.Prelude.Maybe Text
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
