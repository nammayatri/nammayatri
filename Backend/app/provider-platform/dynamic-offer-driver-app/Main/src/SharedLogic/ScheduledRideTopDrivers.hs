{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.ScheduledRideTopDrivers
  ( TopDriversRoundEvent (..),
    topDriversTopic,
    topDriversLeadTime,
    regularRoundStartsAt,
    topDriversRoundCutoff,
    resolveTopDriverRules,
    mkTopDriversRoundEvent,
  )
where

import Domain.Types.ServiceTierType (ServiceTierType)
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Utils.Common
import SharedLogic.ScheduledRideVisibility (resolveVisibilityStage)

defaultMinRating :: Double
defaultMinRating = 4.5

defaultMaxCancellationRatePct :: Double
defaultMaxCancellationRatePct = 10

defaultMinCompletedRides :: Int
defaultMinCompletedRides = 0

topDriversTopic :: Text
topDriversTopic = "scheduled-ride-top-drivers"

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

-- | The visibility rules a driver has to meet to be a top driver for this ride: the rules of the
-- visibility stage that applies, with the defaults filling in whatever the stage leaves out. Only the
-- rules that driver_stats and the driver's position can answer are kept, since that is all the pool
-- calculation knows about a driver. A pickup radius makes the nearby rule apply.
-- The flag is True when the stage set none of the stats rules and the defaults are in force.
resolveTopDriverRules :: DTC.TransporterConfig -> ServiceTierType -> UTCTime -> UTCTime -> (DTC.ScheduledRideVisibilityRules, Bool)
resolveTopDriverRules transporterConfig serviceTier now pickupTime =
  let stages = fromMaybe [] transporterConfig.scheduledRideConfig.scheduledRideVisibilityStages
      mbRules = (.rules) <$> resolveVisibilityStage serviceTier stages (diffUTCTime pickupTime now)
      mbMinRating = mbRules >>= (.minRating)
      mbMaxCancellation = mbRules >>= (.maxCancellationRatePct)
      mbMinRides = mbRules >>= (.minCompletedRides)
      mbRadius = mbRules >>= (.pickupRadiusMeters)
      rules =
        DTC.ScheduledRideVisibilityRules
          Nothing
          (Just (fromMaybe defaultMaxCancellationRatePct mbMaxCancellation))
          (Just (fromMaybe defaultMinCompletedRides mbMinRides))
          (Just (fromMaybe defaultMinRating mbMinRating))
          mbRadius
          Nothing
          (True <$ mbRadius)
          Nothing
   in (rules, isNothing mbMinRating && isNothing mbMaxCancellation && isNothing mbMinRides)

-- | The per-batch event for ClickHouse. The pool is already within the search radius, so the within-radius
-- count equals the candidate count.
mkTopDriversRoundEvent :: Text -> Text -> Int -> Int -> Int -> Int -> [Text] -> DTC.ScheduledRideVisibilityRules -> Bool -> UTCTime -> TopDriversRoundEvent
mkTopDriversRoundEvent searchTryId merchantOperatingCityId batchNumber minutesToPickup candidateCount passedStatsCount selectedDriverIds rules usedDefaults createdAt =
  TopDriversRoundEvent
    { searchTryId,
      merchantOperatingCityId,
      batchNumber,
      minutesToPickup,
      candidateCount,
      withinRadiusCount = candidateCount,
      passedStatsCount,
      selectedCount = length selectedDriverIds,
      selectedDriverIds,
      minRatingRequired = fromMaybe defaultMinRating rules.minRating,
      maxCancellationRatePctAllowed = fromMaybe defaultMaxCancellationRatePct rules.maxCancellationRatePct,
      minCompletedRidesRequired = fromMaybe defaultMinCompletedRides rules.minCompletedRides,
      maxPickupDistanceMeters = rules.pickupRadiusMeters,
      usedDefaults,
      createdAt
    }
