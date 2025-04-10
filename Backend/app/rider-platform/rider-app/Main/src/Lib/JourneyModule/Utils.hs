module Lib.JourneyModule.Utils where

import BecknV2.FRFS.Enums as Spec
import Data.List (sortBy)
import Data.Time hiding (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Domain.Types.IntegratedBPPConfig as DIntegratedBPPConfig
import Domain.Types.Journey
import qualified Domain.Types.Trip as DTrip
import Domain.Utils (utctTimeToDayOfWeek)
import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RouteStopCalender as QRouteCalendar
import qualified Storage.Queries.RouteStopTimeTable as QRouteStopTiming

mapWithIndex :: (MonadFlow m) => (Int -> a -> m b) -> [a] -> m [b]
mapWithIndex f = go 0
  where
    go _ [] = return []
    go idx (x : xs') = do
      y <- f idx x
      ys <- go (idx + 1) xs'
      return (y : ys)

convertMultiModalModeToTripMode :: MultiModal.GeneralVehicleType -> Meters -> Meters -> DTrip.MultimodalTravelMode
convertMultiModalModeToTripMode input distance maximumWalkDistance = case input of
  MultiModal.MetroRail -> DTrip.Metro
  MultiModal.Subway -> DTrip.Subway
  MultiModal.Walk -> if distance > maximumWalkDistance then DTrip.Taxi else DTrip.Walk
  MultiModal.Bus -> DTrip.Bus
  MultiModal.Unspecified -> DTrip.Taxi

mkJourneyUpdateInProgressKey :: Id Journey -> Text
mkJourneyUpdateInProgressKey journeyId = "Journey:UpdateInProgress:JourneyId-" <> journeyId.getId

journeyUpdateInProgress ::
  CacheFlow m r =>
  Id Journey ->
  m Bool
journeyUpdateInProgress journeyId = do
  fromMaybe False <$> (Hedis.withMasterRedis $ Hedis.get (mkJourneyUpdateInProgressKey journeyId))

withJourneyUpdateInProgress ::
  CacheFlow m r =>
  Id Journey ->
  m a ->
  m a
withJourneyUpdateInProgress journeyId actions = do
  Hedis.withMasterRedis $ Hedis.setExp (mkJourneyUpdateInProgressKey journeyId) True 120
  a <- actions
  Hedis.withMasterRedis $ Hedis.setExp (mkJourneyUpdateInProgressKey journeyId) False 120
  return a

whenJourneyUpdateInProgress ::
  CacheFlow m r =>
  Id Journey ->
  m a ->
  m a
whenJourneyUpdateInProgress journeyId actions = do
  gotLock <- journeyUpdateInProgress journeyId
  if gotLock
    then do
      threadDelayMilliSec 200
      whenJourneyUpdateInProgress journeyId actions
    else actions

data UpcomingBusInfo = UpcomingBusInfo
  { routeCode :: Text,
    serviceType :: Spec.ServiceTierType,
    arrivalTimeInSeconds :: Seconds
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Data type representing upcoming trip information
data UpcomingTripInfo = UpcomingTripInfo
  { busFrequency :: Maybe Seconds,
    upcomingBuses :: [UpcomingBusInfo]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Find the top upcoming trips for a given route code and stop code
-- Returns arrival times in seconds for the upcoming trips along with route ID and service type
findUpcomingTrips ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  [Text] ->
  Text ->
  Maybe Spec.ServiceTierType ->
  UTCTime ->
  Id DIntegratedBPPConfig.IntegratedBPPConfig ->
  m UpcomingTripInfo
findUpcomingTrips routeCodes stopCode mbServiceType currentTime integratedBppConfigId = do
  let istOffset :: Double = 5.5 * 3600
      currentTimeIST = addUTCTime (secondsToNominalDiffTime $ round istOffset) currentTime

  routeStopTimings <- QRouteStopTiming.findByRouteCodeAndStopCode routeCodes stopCode integratedBppConfigId

  let filteredByService = case mbServiceType of
        Just serviceType -> filter (\rst -> rst.serviceTierType == serviceType) routeStopTimings
        Nothing -> routeStopTimings

  let tripIds = map (.tripId) filteredByService
  routeCalendars <- QRouteCalendar.findByTripIds tripIds integratedBppConfigId

  let today = fromEnum $ utctTimeToDayOfWeek currentTimeIST
      serviceableTrips = filter (\rc -> if length rc.serviceability > today then (rc.serviceability !! today) > 0 else False) routeCalendars

  -- Combine stop timings with their calendars and get arrival times
  -- Filter out trips that have already passed the stop
  let tripTimingsWithCalendars =
        [ UpcomingBusInfo
            { routeCode = rst.routeCode,
              serviceType = rst.serviceTierType,
              arrivalTimeInSeconds = nominalDiffTimeToSeconds $ diffUTCTime (UTCTime (utctDay currentTimeIST) (timeOfDayToTime rst.timeOfArrival)) currentTimeIST
            }
          | rst <- filteredByService,
            rc <- serviceableTrips,
            rst.tripId == rc.tripId,
            (UTCTime (utctDay currentTimeIST) (timeOfDayToTime rst.timeOfArrival)) > currentTimeIST -- Only include future arrivals
        ]

  let upcomingBuses = sortBy (\a b -> compare (arrivalTimeInSeconds a) (arrivalTimeInSeconds b)) tripTimingsWithCalendars

  let busFrequency =
        if length upcomingBuses >= 3
          then do
            let gaps =
                  zipWith
                    (\a b -> arrivalTimeInSeconds b - arrivalTimeInSeconds a)
                    upcomingBuses
                    (tail upcomingBuses)

            let avgGap :: Double = fromIntegral (sum gaps) / fromIntegral (length gaps)
                gapsAreEqual = all (\gap -> abs (fromIntegral gap - avgGap) <= (0.2 * avgGap)) gaps

            if gapsAreEqual
              then Just $ round avgGap
              else Nothing
          else Nothing

  let upcomingTripInfo =
        UpcomingTripInfo
          { busFrequency = busFrequency,
            upcomingBuses = upcomingBuses
          }
  return upcomingTripInfo
