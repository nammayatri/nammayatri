{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.ScheduledRideTopDrivers
  ( TopDriverCriteria (..),
    TopDriversRoundEvent (..),
    topDriverBatchCount,
    topDriverPoolSize,
    topDriversLeadTime,
    regularRoundStartsAt,
    topDriversRoundCutoff,
    resolveTopDriverCriteria,
    filterTopDrivers,
  )
where

import Data.List (sortOn)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.DriverStats as DStats
import qualified Domain.Types.SearchTry as DST
import Domain.Types.ServiceTierType (ServiceTierType)
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Utils.Common
import SharedLogic.DriverPool (DriverPoolWithActualDistResult (..))
import qualified SharedLogic.DriverPool.Types as Fields
import SharedLogic.ScheduledRideVisibility (resolveVisibilityStage)
import qualified Storage.Queries.DriverStatsExtra as QDriverStats
import qualified Tools.Fields as Fields

topDriverBatchCount :: Int
topDriverBatchCount = 20

topDriverPoolSize :: Int
topDriverPoolSize = 20

defaultMinRating :: Double
defaultMinRating = 4.5

defaultMaxCancellationRatePct :: Double
defaultMaxCancellationRatePct = 10

defaultMinCompletedRides :: Int
defaultMinCompletedRides = 0

topDriversTopic :: Text
topDriversTopic = "scheduled-ride-top-drivers"

data TopDriverCriteria = TopDriverCriteria
  { minRatingRequired :: Double,
    maxCancellationRatePctAllowed :: Double,
    minCompletedRidesRequired :: Int,
    maxPickupDistanceMeters :: Maybe Int,
    usedDefaults :: Bool
  }
  deriving (Show)

data TopDriversRoundEvent = TopDriversRoundEvent
  { searchTryId :: Text,
    merchantOperatingCityId :: Text,
    batchNumber :: Int,
    minutesToPickup :: Int,
    candidateCount :: Int,
    withinRadiusCount :: Int,
    passedStatsCount :: Int,
    selectedCount :: Int,
    selectedDriverIds :: [Text],
    minRatingRequired :: Double,
    maxCancellationRatePctAllowed :: Double,
    minCompletedRidesRequired :: Int,
    maxPickupDistanceMeters :: Maybe Int,
    usedDefaults :: Bool,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

topDriversLeadTime :: DTC.TransporterConfig -> [Int] -> Maybe NominalDiffTime
topDriversLeadTime transporterConfig scheduleTryTimes =
  (\advance -> secondsToNominalDiffTime advance + maybe 0 fromIntegral (listToMaybe scheduleTryTimes)) <$> transporterConfig.scheduledRideConfig.topDriversFirstAdvanceTime

-- | First regular round: the first scheduleTryTime before pickup.
regularRoundStartsAt :: [Int] -> UTCTime -> Maybe UTCTime
regularRoundStartsAt scheduleTryTimes pickupTime =
  (\firstTryTime -> addUTCTime (negate (fromIntegral firstTryTime)) pickupTime) <$> listToMaybe scheduleTryTimes

-- | A top-drivers round must stop one batch interval before the first regular round starts.
topDriversRoundCutoff :: Seconds -> [Int] -> UTCTime -> Maybe UTCTime
topDriversRoundCutoff batchInterval scheduleTryTimes pickupTime =
  addUTCTime (negate (fromIntegral batchInterval)) <$> regularRoundStartsAt scheduleTryTimes pickupTime

resolveTopDriverCriteria :: DTC.TransporterConfig -> ServiceTierType -> UTCTime -> UTCTime -> TopDriverCriteria
resolveTopDriverCriteria transporterConfig serviceTier now pickupTime =
  let stages = fromMaybe [] transporterConfig.scheduledRideConfig.scheduledRideVisibilityStages
      mbRules = (.rules) <$> resolveVisibilityStage serviceTier stages (diffUTCTime pickupTime now)
      mbMinRating = mbRules >>= (.minRating)
      mbMaxCancellation = mbRules >>= (.maxCancellationRatePct)
      mbMinRides = mbRules >>= (.minCompletedRides)
   in TopDriverCriteria
        { minRatingRequired = fromMaybe defaultMinRating mbMinRating,
          maxCancellationRatePctAllowed = fromMaybe defaultMaxCancellationRatePct mbMaxCancellation,
          minCompletedRidesRequired = fromMaybe defaultMinCompletedRides mbMinRides,
          maxPickupDistanceMeters = mbRules >>= (.pickupRadiusMeters),
          usedDefaults = isNothing mbMinRating && isNothing mbMaxCancellation && isNothing mbMinRides
        }

filterTopDrivers ::
  ( MonadFlow m,
    MonadReader r m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasKafkaProducer r
  ) =>
  DST.SearchTry ->
  Int ->
  TopDriverCriteria ->
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
filterTopDrivers searchTry batchNumber criteria pool = do
  now <- getCurrentTime
  let withinRadius = filter (isWithinRadius criteria) pool
  stats <- QDriverStats.findAllByDriverIdListFromDb (map (\dp -> dp.driverPoolResult.driverId) withinRadius)
  let statsByDriver = Map.fromList [(driverStats.driverId, driverStats) | driverStats <- stats]
      passesStats dp = maybe False (isTopDriver criteria) (Map.lookup dp.driverPoolResult.driverId statsByDriver)
      good = filter passesStats withinRadius
      selected = take topDriverPoolSize $ sortOn (\dp -> dp.actualDistanceToPickup) good
  logInfo $
    "TopDrivers: searchTryId=" <> searchTry.id.getId <> " batch=" <> show batchNumber
      <> " candidates="
      <> show (length pool)
      <> " withinRadius="
      <> show (length withinRadius)
      <> " passedStats="
      <> show (length good)
      <> " selected="
      <> show (length selected)
  publishEvent now selected (length withinRadius) (length good)
  pure selected
  where
    publishEvent now selected withinRadiusCount passedStatsCount = do
      let event =
            TopDriversRoundEvent
              { searchTryId = searchTry.id.getId,
                merchantOperatingCityId = searchTry.merchantOperatingCityId.getId,
                batchNumber,
                minutesToPickup = round (diffUTCTime searchTry.startTime now / 60),
                candidateCount = length pool,
                withinRadiusCount,
                passedStatsCount,
                selectedCount = length selected,
                selectedDriverIds = map (\dp -> dp.driverPoolResult.driverId.getId) selected,
                minRatingRequired = criteria.minRatingRequired,
                maxCancellationRatePctAllowed = criteria.maxCancellationRatePctAllowed,
                minCompletedRidesRequired = criteria.minCompletedRidesRequired,
                maxPickupDistanceMeters = criteria.maxPickupDistanceMeters,
                usedDefaults = criteria.usedDefaults,
                createdAt = now
              }
      result <- withTryCatch "publishTopDriversRoundEvent" $ produceMessage (topDriversTopic, Just (TE.encodeUtf8 searchTry.id.getId)) event
      case result of
        Left err -> logError $ "TopDrivers: kafka publish failed for searchTryId=" <> searchTry.id.getId <> ": " <> show err
        Right _ -> pure ()

isWithinRadius :: TopDriverCriteria -> DriverPoolWithActualDistResult -> Bool
isWithinRadius criteria dp = case criteria.maxPickupDistanceMeters of
  Nothing -> True
  Just radius -> getMeters dp.actualDistanceToPickup <= radius

isTopDriver :: TopDriverCriteria -> DStats.DriverStats -> Bool
isTopDriver criteria driverStats = ratingPasses && cancellationPasses && completedRidesPass
  where
    ratingPasses = maybe False (>= criteria.minRatingRequired) (driverRatingOf driverStats)
    cancellationPasses = maybe True (<= criteria.maxCancellationRatePctAllowed) (cancellationRatePctOf driverStats)
    completedRidesPass = driverStats.totalRides >= criteria.minCompletedRidesRequired

driverRatingOf :: DStats.DriverStats -> Maybe Double
driverRatingOf driverStats =
  if fromMaybe 0 driverStats.totalRatings > 0
    then driverStats.rating <&> (\(Centesimal ratingValue) -> realToFrac ratingValue)
    else Nothing

cancellationRatePctOf :: DStats.DriverStats -> Maybe Double
cancellationRatePctOf driverStats = case driverStats.totalRidesAssigned of
  Just assigned | assigned > 0 -> Just (fromIntegral (fromMaybe 0 driverStats.ridesCancelled) * 100 / fromIntegral assigned :: Double)
  _ -> Nothing
