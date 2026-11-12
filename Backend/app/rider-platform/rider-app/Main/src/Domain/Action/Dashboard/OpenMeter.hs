module Domain.Action.Dashboard.OpenMeter
  ( getOpenMeterRideList,
    getOpenMeterRideInfo,
  )
where

import qualified "this" API.Types.RiderPlatform.Management.OpenMeter as Common
import qualified Dashboard.Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.OpenMeterRide as DOMR
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.OpenMeterRide as QOMR
import Tools.Error

defaultRidesLimit :: Int
defaultRidesLimit = 20

maxRidesLimit :: Int
maxRidesLimit = 100

-- | The city's open meter rides, newest first, a page at a time. The total is not counted: like the
-- ride list, totalCount is a fixed upper bound so the dashboard keeps offering the next page.
getOpenMeterRideList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe (Id Dashboard.Common.Customer) ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe DOMR.OpenMeterRideStatus ->
  Maybe UTCTime ->
  Flow Common.OpenMeterRideListRes
getOpenMeterRideList merchantShortId opCity mbCustomerId mbFrom mbLimit mbOffset mbStatus mbTo = do
  merchantOpCityId <- findMerchantOpCityId merchantShortId opCity
  let limit = min maxRidesLimit . max 1 $ fromMaybe defaultRidesLimit mbLimit
      offset = max 0 $ fromMaybe 0 mbOffset
  rides <- QOMR.findAllByMerchantOpCityIdPaginated merchantOpCityId (cast <$> mbCustomerId) mbStatus mbFrom mbTo limit offset
  let pageCount = length rides
  pure
    Common.OpenMeterRideListRes
      { totalItems = pageCount,
        summary = Dashboard.Common.Summary {totalCount = 10000, count = pageCount},
        rides = map mkListItem rides
      }

-- | A ride of another city is reported exactly like a missing one.
getOpenMeterRideInfo :: ShortId DM.Merchant -> Context.City -> Id DOMR.OpenMeterRide -> Flow Common.OpenMeterRideInfoRes
getOpenMeterRideInfo merchantShortId opCity rideId = do
  merchantOpCityId <- findMerchantOpCityId merchantShortId opCity
  ride <- QOMR.findByPrimaryKey rideId >>= fromMaybeM (OpenMeterRideNotFound rideId.getId)
  unless (ride.merchantOperatingCityId == Just merchantOpCityId) $ throwError (OpenMeterRideNotFound rideId.getId)
  pure
    Common.OpenMeterRideInfoRes
      { rideId = ride.id,
        customerId = cast ride.personId,
        status = ride.status,
        startTime = ride.startTime,
        serverStartedAt = ride.serverStartedAt,
        endTime = ride.endTime,
        lastHeartbeatAt = ride.lastHeartbeatAt,
        startLatLong = ride.startLatLong,
        endLatLong = ride.endLatLong,
        lastLatLong = ride.lastLatLong,
        distanceMeters = ride.distanceMeters,
        serverLinearDistanceMeters = ride.serverLinearDistanceMeters,
        estimatedGapMeters = ride.estimatedGapMeters,
        waitingTimeSeconds = ride.waitingTimeSeconds,
        encodedPolyline = ride.encodedPolyline,
        pointsCount = ride.pointsCount,
        fare = ride.fare,
        currency = ride.currency,
        tariffSnapshot = ride.tariffSnapshot,
        trackingConfig = ride.trackingConfig,
        endReason = ride.endReason,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt
      }

mkListItem :: DOMR.OpenMeterRide -> Common.OpenMeterRideListItem
mkListItem ride =
  Common.OpenMeterRideListItem
    { rideId = ride.id,
      customerId = cast ride.personId,
      status = ride.status,
      startTime = ride.startTime,
      endTime = ride.endTime,
      distanceMeters = ride.distanceMeters,
      serverLinearDistanceMeters = ride.serverLinearDistanceMeters,
      waitingTimeSeconds = ride.waitingTimeSeconds,
      fare = ride.fare,
      currency = ride.currency,
      endReason = ride.endReason,
      createdAt = ride.createdAt
    }

findMerchantOpCityId :: ShortId DM.Merchant -> Context.City -> Flow (Id DMOC.MerchantOperatingCity)
findMerchantOpCityId merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  pure merchantOpCity.id
