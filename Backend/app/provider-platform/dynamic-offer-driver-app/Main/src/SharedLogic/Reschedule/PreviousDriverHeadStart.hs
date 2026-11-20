{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Rider reschedule: the driver of the rescheduled booking is offered the new ride alone first.
--
-- The offer is built here, outside the driver pool: the regular pool is left untouched and only
-- runs once his window has passed (see 'SharedLogic.SearchTry.initiateDriverSearchBatch').
module SharedLogic.Reschedule.PreviousDriverHeadStart
  ( previousDriverHeadStartWindow,
    claimPreviousDriverHeadStart,
    releasePreviousDriverHeadStart,
    previousDriverEntry,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Domain.Types (ServiceTierType)
import Domain.Types.DriverIntelligentPoolConfig (IntelligentScores (..))
import Domain.Types.DriverLocation (DriverLocation (..))
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.TransporterConfig (TransporterConfig (..))
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common (Seconds (..))
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common (CacheFlow, MonadFlow, diffUTCTime, getCurrentTime)
import qualified SharedLogic.DriverPool.DriverPoolData as DPD
import SharedLogic.DriverPool.Types (DriverPoolResult (..), DriverPoolWithActualDistResult (..))
import SharedLogic.Subscription.BillingModel (isExemptFromPostpaidDuesFlag)
import qualified Storage.Queries.Person.GetNearestDrivers as QPG

-- | How long the driver of the rescheduled booking has the new ride to himself, right after it is
-- confirmed, before anyone else is offered it.
previousDriverHeadStartWindow :: Seconds
previousDriverHeadStartWindow = Seconds 60

-- | The head start is given at most once per search: only the caller that wins this claim offers
-- it. The claim lasts as long as the search can be dispatched. A failed head-start dispatch gives
-- it back ('releasePreviousDriverHeadStart') so a later dispatch can retry.
claimPreviousDriverHeadStart :: (MonadFlow m, CacheFlow m r) => DSR.SearchRequest -> m Bool
claimPreviousDriverHeadStart searchReq = do
  now <- getCurrentTime
  let ttl = max 0 (ceiling (diffUTCTime searchReq.validTill now)) + 3600
  Redis.setNxExpire (previousDriverHeadStartClaimKey searchReq.id) ttl True

releasePreviousDriverHeadStart :: (MonadFlow m, CacheFlow m r) => Id DSR.SearchRequest -> m ()
releasePreviousDriverHeadStart searchReqId = Redis.del (previousDriverHeadStartClaimKey searchReqId)

previousDriverHeadStartClaimKey :: Id DSR.SearchRequest -> Text
previousDriverHeadStartClaimKey searchReqId = "Driver:Reschedule:PreviousDriverClaim:SearchReqId-" <> searchReqId.getId

-- | How old his last reported position may be: an app that stopped reporting cannot hold the ride.
maxLocationAge :: NominalDiffTime
maxLocationAge = 600

-- | Rough speed for his pickup time: he is offered a ride hours away, so this is display only.
headStartPickupSpeedMetersPerSecond :: Double
headStartPickupSpeedMetersPerSecond = 5

-- | The previous driver's entry for the head-start offer, from his cached pool data and live
-- position, when he can take the ride: enabled, not blocked or disabled, dues clear, online and
-- not on a ride, a recent position, and still offering the booking's vehicle tier. Nothing
-- otherwise; the ride is then allocated as usual.
previousDriverEntry ::
  UTCTime ->
  TransporterConfig ->
  [DVST.VehicleServiceTier] ->
  Bool ->
  DSR.SearchRequest ->
  ServiceTierType ->
  DPD.DriverPoolData ->
  DriverLocation ->
  Maybe DriverPoolWithActualDistResult
previousDriverEntry now transporterConfig cityServiceTiers isPrepaidEnabled searchReq serviceTier dpd location = do
  guard $ diffUTCTime now location.coordinatesCalculatedAt <= maxLocationAge
  guard $ dpd.enabled && not dpd.blocked && not (fromMaybe False dpd.isDisabledReasonFlag)
  guard $ dpd.subscribed || isExemptFromPostpaidDuesFlag isPrepaidEnabled dpd.fleetOwnerId dpd.rideBillingModel
  guard $ QPG.isDriverModeEligibleHelper dpd.mode dpd.active
  guard $ not dpd.onRide
  guard $ serviceTier `elem` dpd.selectedServiceTiers && serviceTier `notElem` fromMaybe [] dpd.softBlockStiers
  let pickup = LatLong {lat = searchReq.fromLocation.lat, lon = searchReq.fromLocation.lon}
      distance = realToFrac (distanceBetweenInMeters pickup (LatLong {lat = location.lat, lon = location.lon})) :: Double
      cityServiceTiersMap = HashMap.fromList [(vst.serviceTierType, vst) | vst <- cityServiceTiers]
      mbDefaultServiceTier = find (\vst -> dpd.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
  result <- QPG.mkResultHelper now dpd location distance mbDefaultServiceTier cityServiceTiersMap Nothing Nothing Nothing serviceTier
  pure
    DriverPoolWithActualDistResult
      { driverPoolResult = toDriverPoolResult result,
        actualDistanceToPickup = roundToIntegral distance,
        actualDurationToPickup = Seconds (roundToIntegral (distance / headStartPickupSpeedMetersPerSecond)),
        intelligentScores = IntelligentScores Nothing Nothing Nothing Nothing Nothing Nothing transporterConfig.defaultPopupDelay,
        isPartOfIntelligentPool = False,
        pickupZone = False,
        specialZoneExtraTip = Nothing,
        searchTags = Nothing,
        tripDistance = Nothing,
        keepHiddenForSeconds = Seconds 0,
        goHomeReqId = Nothing,
        isForwardRequest = False,
        previousDropGeoHash = Nothing,
        specialLocWarriorPreferredSpecialLocId = Nothing,
        score = Nothing,
        poolingLogicVersion = Nothing,
        searchReqDriverStatsCounters = Nothing,
        idleTimeSeconds = Nothing,
        preferenceMatchScore = 1.0,
        hasApplicablePreferences = False,
        fleetPoolingPriorityLevel = Nothing
      }

-- | Same mapping as the regular pool's, for a driver who is not on a ride.
toDriverPoolResult :: QPG.NearestDriversResult -> DriverPoolResult
toDriverPoolResult QPG.NearestDriversResult {..} =
  DriverPoolResult
    { distanceToPickup = distanceToDriver,
      customerTags = Nothing,
      minRideDistance = tripDistanceMinThreshold,
      maxRideDistance = tripDistanceMaxThreshold,
      driverGender = Just driverGender,
      vehicleNumber,
      reactBundleVersion = Nothing,
      backendConfigVersion = Nothing,
      backendAppVersion = Nothing,
      onRide = Just onRide,
      ..
    }
