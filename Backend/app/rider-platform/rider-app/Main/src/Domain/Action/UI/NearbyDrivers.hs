{-# LANGUAGE BangPatterns #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.NearbyDrivers (postNearbyDrivers) where

import qualified API.Types.UI.NearbyBuses as NB
import qualified API.Types.UI.NearbyDrivers as ND
import qualified BecknV2.FRFS.Enums as Spe
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as Map
import Domain.Action.UI.NearbyBuses (getSimpleNearbyBuses)
import Domain.Action.UI.Serviceability (getNearestOperatingAndCurrentCity)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.Person as DP
import Domain.Types.RiderConfig (RingBucketCfg)
import qualified Domain.Types.RiderConfig as DomainRiderConfig
import qualified Domain.Types.ServiceTierType as DST
import qualified Domain.Types.Trip as DTrip
import qualified Domain.Types.VehicleVariant as DV
import Domain.Utils (safeLast)
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.HasCoordinates as Maps
import qualified Kernel.External.Maps.Types as MapsTypes
import qualified Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id)
import qualified Kernel.Types.Logging as Log
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common (CacheFlow, HasFlowEnv, Meters (..), MonadFlow, fromMaybeM, logDebug, throwError, type (:::))
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LTSTypes
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import Tools.Error

deriving instance Hashable Meters

data RingBucketKey = RingBucketKey
  { radius :: Meters,
    variant :: DV.VehicleVariant
  }
  deriving (Show, Eq, Generic, Hashable, Ord)

type VVToSTT = Map DV.VehicleVariant [DST.ServiceTierType]

postNearbyDrivers ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  ND.NearbyDriverReq ->
  Environment.Flow ND.NearbyDriverRes
postNearbyDrivers (Nothing, _) _ = do
  throwError $ InternalError "PersonId is not present in the request"
postNearbyDrivers (Just personId, merchantId) req = withLogTag $ do
  checkRateLimit nearByDriverHitsCountKey
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound $ merchantId.getId)
  cityRes <- getNearestOperatingAndCurrentCity (.origin) (personId, merchantId) False req.location
  let reqCity = cityRes.currentCity.city
  moc <- CQMOC.findByMerchantIdAndCity merchantId reqCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId:" <> merchantId.getId <> "city:" <> show reqCity)
  riderConfig <- CQRC.findByMerchantOperatingCityId moc.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist moc.id.getId)
  case req.travelMode of
    Just DTrip.Bus -> do
      vehicleDataBuckets <- getNearbyBusesAsBuckets moc.id riderConfig req.location
      return $
        ND.NearbyDriverRes
          { serviceTierTypeToVehicleVariant = A.Null,
            variantLevelDriverCount = A.Null,
            buckets = [],
            vehicleDataBuckets = vehicleDataBuckets
          }
    _ -> do
      variantListForNearByReq <- (req.vehicleVariants <|> riderConfig.variantListForNearByReq) & fromMaybeM (RiderConfigFieldIsEmpty "variantListForNearByReq" moc.id.getId)
      ringBucketCfg' <- riderConfig.nearByDriverRingBucketCfg & fromMaybeM (RiderConfigFieldIsEmpty "nearByDriverRingBucketCfg" moc.id.getId)
      let sortedRingBucketCfgs = sortOn (.radiusInMeters) ringBucketCfg'
      maxRadiusBucket <- safeLast sortedRingBucketCfgs & fromMaybeM (RiderConfigFieldIsEmpty "nearByDriverRingBucketCfg list is empty" moc.id.getId)
      nearbyDriverLocations <- LF.nearBy $ buildNearByReq merchant variantListForNearByReq (min maxRadiusBucket.radiusInMeters req.radius)
      let vvToSttMapping = getVariantToApplicableServiceTierTypeMapping
      let sttToVvMapping = getServiceTierTypeToVariantMapping
      buckets <- buildBuckets vvToSttMapping sortedRingBucketCfgs nearbyDriverLocations
      let variantLevelCountMap =
            foldr'
              ( \driverLoc acc -> do
                  let variant = driverLoc.vehicleType
                  let !existingCount = Map.findWithDefault 0 variant acc
                  Map.insert variant (existingCount + 1) acc
              )
              (Map.empty :: Map DV.VehicleVariant Int)
              nearbyDriverLocations

      let vehicleDataBuckets =
            map
              ( \bucket ->
                  ND.VehicleDataBucket
                    { radius = bucket.radius,
                      travelMode = DTrip.Taxi,
                      vehicleInfo = ND.Taxi bucket
                    }
              )
              buckets

      return $
        ND.NearbyDriverRes
          { serviceTierTypeToVehicleVariant = transformToObject sttToVvMapping,
            variantLevelDriverCount = transformToObject variantLevelCountMap,
            buckets,
            vehicleDataBuckets = vehicleDataBuckets
          }
  where
    nearByDriverHitsCountKey :: Text
    nearByDriverHitsCountKey = "BAP:API:UI:NearByDriver:PersonId:" <> personId.getId <> ":hitsCount"

    transformToObject :: (Show a, A.ToJSON b) => Map a b -> A.Value
    transformToObject = A.Object . KM.fromList . map (bimap (A.fromText . show) A.toJSON) . Map.toList

    buildNearByReq merchant vehicleTypes radius = do
      LTSTypes.NearByDriverReq
        { lat = req.location.lat,
          lon = req.location.lon,
          onRide = Nothing,
          vehicleType = Just vehicleTypes,
          radius,
          merchantId = merchant.driverOfferMerchantId,
          groupId = Nothing
        }

    buildBuckets :: MonadFlow m => VVToSTT -> [RingBucketCfg] -> [LTSTypes.NearByDriverRes] -> m [ND.NearByDriversBucket]
    buildBuckets vvToSttMapping sortedRingBucketCfgs driverLocs = do
      res <- buildBuckets' vvToSttMapping sortedRingBucketCfgs driverLocs
      return . map (\(bucketKey, info) -> ND.NearByDriversBucket {radius = bucketKey.radius, variant = bucketKey.variant, driverInfo = info}) $ Map.toList res

    buildBuckets' :: MonadFlow m => VVToSTT -> [RingBucketCfg] -> [LTSTypes.NearByDriverRes] -> m (Map RingBucketKey [ND.DriverInfo])
    buildBuckets' vvToSttMapping sortedRingBucketCfgs =
      foldrM
        ( \driverLoc acc -> do
            let distanceInMeters = round $ distanceBetweenInMeters (Maps.getCoordinates driverLoc) req.location
            let bucket' = bucketForRadius sortedRingBucketCfgs driverLoc.vehicleType distanceInMeters
            case bucket' of
              Nothing -> do
                logDebug $ "No bucket found for distance:" <> show distanceInMeters <> ", and vehicleType:" <> show driverLoc.vehicleType
                pure acc
              Just bucket -> do
                let key = RingBucketKey {radius = bucket.radiusInMeters, variant = driverLoc.vehicleType}
                let driverInfo = buildDriverInfo vvToSttMapping distanceInMeters driverLoc
                let !existingInfo = Map.findWithDefault [] key acc
                pure $ bool acc (Map.insert key (driverInfo : existingInfo) acc) (length existingInfo < bucket.size)
        )
        Map.empty

    buildDriverInfo :: VVToSTT -> Meters -> LTSTypes.NearByDriverRes -> ND.DriverInfo
    buildDriverInfo vvToSttMapping distanceInMeters driverLoc = do
      ND.DriverInfo
        { lat = driverLoc.lat,
          lon = driverLoc.lon,
          applicableServiceTierTypes = Map.findWithDefault [] driverLoc.vehicleType vvToSttMapping,
          distance = distanceInMeters,
          driverId = driverLoc.driverId.getId,
          bearing = round <$> driverLoc.bear,
          rideDetails = (\rideDetails -> ND.RideDetails {rideId = rideDetails.rideId, rideInfo = buildRideInfo <$> rideDetails.rideInfo}) <$> driverLoc.rideDetails
        }

    buildRideInfo :: LTSTypes.RideInfo -> ND.RideInfo
    buildRideInfo = \case
      LTSTypes.Bus LTSTypes.BusRideInfo {..} -> ND.Bus ND.BusRideInfo {..}
      LTSTypes.Car LTSTypes.CarRideInfo {..} -> ND.Car ND.CarRideInfo {..}

    bucketForRadius :: [RingBucketCfg] -> DV.VehicleVariant -> Meters -> Maybe RingBucketCfg
    bucketForRadius sortedRingBucketCfgs vehVariant radius = do
      bucket <- find (\cfg -> cfg.vehVariant == vehVariant && cfg.radiusInMeters >= radius) sortedRingBucketCfgs
      return bucket

    withLogTag = Log.withLogTag ("BAP:API:UI:NearByDriver:" <> personId.getId)

getVariantToApplicableServiceTierTypeMapping :: VVToSTT
getVariantToApplicableServiceTierTypeMapping =
  foldr'
    ( \tierType acc -> do
        let variant = DV.castServiceTierToVariant tierType
        let !existingTiersTypes = Map.findWithDefault [] variant acc
        Map.insert variant (tierType : existingTiersTypes) acc
    )
    Map.empty
    DST.allServiceTiersTypes

getServiceTierTypeToVariantMapping :: Map DST.ServiceTierType DV.VehicleVariant
getServiceTierTypeToVariantMapping =
  foldr'
    ( \tierType acc -> do
        let variant = DV.castServiceTierToVariant tierType
        Map.insert tierType variant acc
    )
    Map.empty
    DST.allServiceTiersTypes

checkRateLimit ::
  ( Redis.HedisFlow m r,
    CacheFlow m r,
    DB.EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    HasFlowEnv m r '["nearByDriverAPIRateLimitOptions" ::: APIRateLimitOptions]
  ) =>
  Text ->
  m ()
checkRateLimit hitsCountKey = do
  rateLimitOptions <- asks (.nearByDriverAPIRateLimitOptions)
  checkSlidingWindowLimitWithOptions hitsCountKey rateLimitOptions

getNearbyBusesAsBuckets ::
  Id MerchantOperatingCity ->
  DomainRiderConfig.RiderConfig ->
  MapsTypes.LatLong ->
  Environment.Flow [ND.VehicleDataBucket]
getNearbyBusesAsBuckets merchantOperatingCityId riderConfig location = do
  let nearbyBusesReq =
        NB.NearbyBusesRequest
          { userLat = location.lat,
            userLon = location.lon,
            vehicleType = Spe.BUS,
            platformType = DIBC.MULTIMODAL,
            requireNearbyBuses = True,
            requireRecentRide = False
          }

  nearbyBuses <- getSimpleNearbyBuses merchantOperatingCityId riderConfig nearbyBusesReq
  logDebug $ "NearbyDrivers: Got " <> show (length nearbyBuses) <> " nearby buses"

  -- Convert NearbyBus to PublicTransportInfo with distance calculation
  let busesWithDistance =
        map
          ( \bus ->
              let dist = case bus.distance of
                    Just d -> d
                    Nothing ->
                      let busLatLong = bus.currentLocation
                       in realToFrac $ distanceBetweenInMeters location busLatLong
               in ( bus,
                    ND.PublicTransportInfo
                      { vehicleNumber = bus.vehicleNumber,
                        routeCode = bus.routeCode,
                        currentLocation = bus.currentLocation,
                        distance = Just dist,
                        shortName = bus.shortName,
                        routeState = bus.routeState,
                        bearing = bus.bearing,
                        serviceSubTypes = bus.serviceSubTypes
                      },
                    dist
                  )
          )
          nearbyBuses

  let nearbyBusRadiusBucketStep :: Meters = fromMaybe 100 riderConfig.nearbyBusRadiusBucketStep

  let nearbyBusSearchRadiusKm :: Double = fromMaybe 0.5 riderConfig.nearbyBusSearchRadius
  let nearbyBusSearchRadiusMeters :: Meters = Meters . round $ nearbyBusSearchRadiusKm * 1000

  let radiusBuckets = generateRadiusBuckets nearbyBusRadiusBucketStep nearbyBusSearchRadiusMeters

  let groupedBucketsWithBuses = groupByRadiusAndServiceType radiusBuckets busesWithDistance

  let vehicleDataBuckets =
        map
          ( \((bucketRadius, mbServiceType, mbServiceTierName), buses) ->
              ND.VehicleDataBucket
                { radius = bucketRadius,
                  travelMode = DTrip.Bus,
                  vehicleInfo =
                    ND.PublicTransport $
                      ND.PublicTransportBucket
                        { serviceType = mbServiceType,
                          serviceTierName = mbServiceTierName,
                          vehicles = sortOn (.distance) buses
                        }
                }
          )
          groupedBucketsWithBuses

  return vehicleDataBuckets
  where
    generateRadiusBuckets :: Meters -> Meters -> [Meters]
    generateRadiusBuckets step maxRadius =
      takeWhile (<= maxRadius) [step, step + step ..]

    findBucket :: [Meters] -> Double -> Maybe Meters
    findBucket buckets dist =
      let distMeters = Meters (round dist)
       in find (>= distMeters) buckets

    groupByRadiusAndServiceType ::
      [Meters] ->
      [(NB.NearbyBus, ND.PublicTransportInfo, Double)] ->
      [((Meters, Maybe Spe.ServiceTierType, Maybe Text), [ND.PublicTransportInfo])]
    groupByRadiusAndServiceType buckets buses =
      let grouped =
            foldr
              ( \(bus, info, dist) acc ->
                  case findBucket buckets dist of
                    Nothing -> acc
                    Just bucket ->
                      let key = (bucket, bus.serviceType, bus.serviceTierName)
                       in Map.insertWith (++) key [info] acc
              )
              Map.empty
              buses
       in Map.toList grouped
