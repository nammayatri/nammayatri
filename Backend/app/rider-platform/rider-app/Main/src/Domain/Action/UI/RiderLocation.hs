{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.RiderLocation (postIdentifyNearByBus) where

import qualified API.Types.UI.RiderLocation
import qualified Data.Aeson as Aeson
import Data.OpenApi (ToSchema)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.RiderConfig as DomainRiderConfig
import qualified Environment
import qualified EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.CalculateDistance
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import Tools.Auth
import Tools.Error

postIdentifyNearByBus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.RiderLocation.RiderLocationRequest ->
    Environment.Flow API.Types.UI.RiderLocation.RiderLocationResponse
  )
postIdentifyNearByBus (_mbPersonId, merchantId) req = do
  _merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  merchantOperatingCity <- CQMOC.findByMerchantIdAndCity merchantId req.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show req.city)
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCity.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
  let riderLocation = Kernel.External.Maps.Types.LatLong req.riderLat req.riderLon
  nearbyBuses <- getNearbyBuses riderLocation riderConfig
  busLocations <- mapM (convertToBusLocation riderLocation) nearbyBuses

  return $ API.Types.UI.RiderLocation.RiderLocationResponse {buses = busLocations}
  where
    getNearbyBuses :: Kernel.External.Maps.Types.LatLong -> DomainRiderConfig.RiderConfig -> Environment.Flow [CQMMB.BusDataWithRoutesInfo]
    getNearbyBuses userPos riderConfig = do
      let nearbyBusSearchRadius :: Double = fromMaybe 0.1 riderConfig.nearbyBusSearchRadius
          maxNearbyBuses :: Int = fromMaybe 5 riderConfig.maxNearbyBuses
      busesBS <- mapM (pure . decodeUtf8) =<< (CQMMB.withCrossAppRedisNew $ Hedis.geoSearch nearbyBusKey (Hedis.FromLonLat userPos.lon userPos.lat) (Hedis.ByRadius nearbyBusSearchRadius "km"))
      logDebug $ "getNearbyBuses: busesBS: " <> show busesBS
      buses <-
        if null busesBS
          then do
            logDebug $ "getNearbyBuses: No buses found in geo search, returning empty list"
            pure []
          else do
            logDebug $ "getNearbyBuses: Fetching bus metadata for " <> show (length busesBS) <> " buses"
            CQMMB.withCrossAppRedisNew $ Hedis.hmGet vehicleMetaKey busesBS
      let validBuses = catMaybes buses
          sortedLimitedBuses = take maxNearbyBuses $ EulerHS.Prelude.sortOn (distanceToUser userPos) validBuses
      logDebug $ "getNearbyBuses: returning " <> show (length sortedLimitedBuses) <> " nearest buses out of " <> show (length validBuses)
      pure sortedLimitedBuses
      where
        distanceToUser :: Kernel.External.Maps.Types.LatLong -> CQMMB.BusDataWithRoutesInfo -> Double
        distanceToUser riderLoc busData = realToFrac $ Kernel.Utils.CalculateDistance.distanceBetweenInMeters riderLoc (Kernel.External.Maps.Types.LatLong busData.latitude busData.longitude)

    convertToBusLocation :: Kernel.External.Maps.Types.LatLong -> CQMMB.BusDataWithRoutesInfo -> Environment.Flow API.Types.UI.RiderLocation.BusLocation
    convertToBusLocation riderLocation busData = do
      let busLocation = Kernel.External.Maps.Types.LatLong busData.latitude busData.longitude
      let distanceToBus = realToFrac $ Kernel.Utils.CalculateDistance.distanceBetweenInMeters riderLocation busLocation
      let busTimestamp = posixSecondsToUTCTime (fromIntegral busData.timestamp)
      let busNumber = fromMaybe "UNKNOWN" busData.vehicle_number
      now <- getCurrentTime
      locationId <- generateGUID
      pure
        API.Types.UI.RiderLocation.BusLocation
          { id = locationId,
            busNumber = busNumber,
            distanceToBus = distanceToBus,
            timestamp = busTimestamp,
            customerLocation = riderLocation,
            customerLocationTimestamp = now,
            locationAccuracy = req.locationAccuracy
          }

    nearbyBusKey :: Text
    nearbyBusKey = "bus_locations"

    vehicleMetaKey :: Text
    vehicleMetaKey = "bus_metadata_v2"
