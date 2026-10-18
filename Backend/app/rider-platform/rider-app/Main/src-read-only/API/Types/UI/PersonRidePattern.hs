{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PersonRidePattern where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Extra.PersonRidePatternSummary
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data DataWindow = DataWindow {unit :: WindowUnit, value :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RidePatternSummaryResp = RidePatternSummaryResp
  { cancelledTaxiRides :: Kernel.Prelude.Int,
    completedTaxiRides :: Kernel.Prelude.Int,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    dataFor :: DataWindow,
    dayOfWeekCounts :: [Kernel.Prelude.Int],
    freeFormSummary :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hourOfDayCounts :: [Kernel.Prelude.Int],
    lastAnalysedAt :: Kernel.Prelude.UTCTime,
    lastSummarizedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastTripAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    medianFareByService :: Domain.Types.Extra.PersonRidePatternSummary.MoneyByServiceMap,
    medianTipValue :: Domain.Types.Extra.PersonRidePatternSummary.MoneyByServiceMap,
    publicTransportTrips :: Kernel.Prelude.Int,
    routePatterns :: Domain.Types.Extra.PersonRidePatternSummary.RoutePatternMap,
    scheduledTaxiRides :: Kernel.Prelude.Int,
    serviceTakenCounts :: Domain.Types.Extra.PersonRidePatternSummary.ServiceCountMap,
    tipComfortability :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    totalTrips :: Kernel.Prelude.Int,
    uniqueRoutesTravelled :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideSuggestionsResp = RideSuggestionsResp
  { dataFor :: Kernel.Prelude.Maybe DataWindow,
    lastAnalysedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    preferredService :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    suggestions :: [RouteSuggestion]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteSuggestion = RouteSuggestion
  { currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    fromAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    fromLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    fromStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reasons :: [SuggestionReason],
    routeKey :: Kernel.Prelude.Text,
    score :: Kernel.Prelude.Double,
    suggestedService :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    suggestedTip :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    toAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    toLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    toStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    travelMode :: Domain.Types.Extra.PersonRidePatternSummary.TravelMode,
    tripsInWindow :: Kernel.Prelude.Int,
    typicalFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    usualTimeOfDay :: [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SuggestionReason
  = FREQUENT_ROUTE
  | USUAL_TIME
  | NEAR_ORIGIN
  | RECENTLY_TRAVELLED
  | SAME_DAY_TYPE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WindowUnit
  = Days
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
