module Domain.Action.UI.PersonRidePattern
  ( getRidePatternSummary,
    getRidePatternSuggestions,
  )
where

import qualified API.Types.UI.PersonRidePattern as API
import Control.Applicative ((<|>))
import qualified Data.Map.Strict as M
import Data.Time (UTCTime (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.PersonRidePatternSummary as DPRPS
import qualified Environment
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.PersonRidePatternSummary as SPRPS

defaultSuggestionLimit :: Int
defaultSuggestionLimit = 3

maxSuggestionLimit :: Int
maxSuggestionLimit = 10

minTipComfortabilityForSuggestion :: Double
minTipComfortabilityForSuggestion = 50

getRidePatternSummary ::
  ( Maybe (Id Domain.Types.Person.Person),
    Id Domain.Types.Merchant.Merchant
  ) ->
  Environment.Flow API.RidePatternSummaryResp
getRidePatternSummary (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  summary <- SPRPS.getOrComputeSummary personId >>= fromMaybeM (InvalidRequest "Ride pattern summary not available for this user yet")
  fork "refresh ride pattern text summary" $ SPRPS.refreshFreeFormSummaryIfStale summary
  pure
    API.RidePatternSummaryResp
      { dataFor = mkDataWindow summary,
        totalTrips = summary.totalTrips,
        completedTaxiRides = summary.completedTaxiRides,
        cancelledTaxiRides = summary.cancelledTaxiRides,
        scheduledTaxiRides = summary.scheduledTaxiRides,
        publicTransportTrips = summary.publicTransportTrips,
        serviceTakenCounts = summary.serviceTakenCounts,
        uniqueRoutesTravelled = summary.uniqueRoutesTravelled,
        routePatterns = summary.routePatterns,
        tipComfortability = summary.tipComfortability,
        medianTipValue = summary.medianTipValue,
        medianFareByService = summary.medianFareByService,
        hourOfDayCounts = summary.hourOfDayCounts,
        dayOfWeekCounts = summary.dayOfWeekCounts,
        lastTripAt = summary.lastTripAt,
        currency = summary.currency,
        freeFormSummary = summary.freeFormSummary,
        lastSummarizedAt = summary.lastSummarizedAt,
        lastAnalysedAt = summary.updatedAt
      }

getRidePatternSuggestions ::
  ( Maybe (Id Domain.Types.Person.Person),
    Id Domain.Types.Merchant.Merchant
  ) ->
  Maybe Double ->
  Maybe Double ->
  Maybe Int ->
  Environment.Flow API.RideSuggestionsResp
getRidePatternSuggestions (mbPersonId, _) mbLat mbLon mbLimit = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  SPRPS.getOrComputeSummary personId >>= \case
    Nothing ->
      pure API.RideSuggestionsResp {suggestions = [], preferredService = Nothing, dataFor = Nothing, lastAnalysedAt = Nothing}
    Just summary -> do
      fork "refresh ride pattern text summary" $ SPRPS.refreshFreeFormSummaryIfStale summary
      now <- getCurrentTime
      offset <- SPRPS.getLocalTimeOffset summary.merchantOperatingCityId
      let ctx =
            SPRPS.SuggestionContext
              { localNow = addUTCTime offset now,
                windowDays = summary.analysisWindowDays,
                windowEndLocalDay = utctDay (addUTCTime offset summary.windowEnd),
                origin = LatLong <$> mbLat <*> mbLon
              }
          limit = max 1 . min maxSuggestionLimit $ fromMaybe defaultSuggestionLimit mbLimit
          ranked = take limit $ SPRPS.rankRoutes ctx summary.routePatterns
      pure
        API.RideSuggestionsResp
          { suggestions = map (mkSuggestion summary) ranked,
            preferredService = SPRPS.topService summary.serviceTakenCounts,
            dataFor = Just $ mkDataWindow summary,
            lastAnalysedAt = Just summary.updatedAt
          }

mkSuggestion :: DPRPS.PersonRidePatternSummary -> SPRPS.ScoredRoute -> API.RouteSuggestion
mkSuggestion summary scored =
  let routePattern = scored.routePattern
      service = SPRPS.topService routePattern.serviceCounts
      tipComfortable = maybe False (>= minTipComfortabilityForSuggestion) summary.tipComfortability
      suggestedTip =
        if routePattern.travelMode == DPRPS.TAXI && tipComfortable
          then service >>= (`M.lookup` summary.medianTipValue)
          else Nothing
   in API.RouteSuggestion
        { routeKey = scored.routeKey,
          travelMode = routePattern.travelMode,
          fromLat = routePattern.fromLat,
          fromLon = routePattern.fromLon,
          toLat = routePattern.toLat,
          toLon = routePattern.toLon,
          fromAddress = routePattern.fromAddress,
          toAddress = routePattern.toAddress,
          fromStopCode = routePattern.fromStopCode,
          toStopCode = routePattern.toStopCode,
          suggestedService = service,
          suggestedTip = suggestedTip,
          typicalFare = routePattern.medianFare <|> (service >>= (`M.lookup` summary.medianFareByService)),
          currency = summary.currency,
          usualTimeOfDay = routePattern.timeOfDay,
          tripsInWindow = routePattern.count,
          score = scored.score,
          reasons = scored.reasons
        }

mkDataWindow :: DPRPS.PersonRidePatternSummary -> API.DataWindow
mkDataWindow summary = API.DataWindow {value = summary.analysisWindowDays, unit = API.Days}
