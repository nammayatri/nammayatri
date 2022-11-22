module Domain.Action.Beckn.Search where

import Beckn.Prelude
import Beckn.Serviceability
import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.List
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import Domain.Types.FarePolicy (FarePolicy)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.Vehicle.Variant as Variant
import Environment
import qualified EulerHS.Language as L
import SharedLogic.DriverPool
import SharedLogic.FareCalculator
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Geometry as QGeometry
import Storage.Queries.Person (DriverPoolResult)
import qualified Storage.Queries.Person as QP
import System.Random
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics.ARDUBPPMetrics as Metrics

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Maybe Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    dropLocation :: DLoc.SearchReqLocationAPIEntity
  }

data DSearchRes = DSearchRes
  { provider :: DM.Merchant,
    fromLocation :: DLoc.SearchReqLocation,
    toLocation :: DLoc.SearchReqLocation,
    now :: UTCTime,
    estimateList :: [EstimateItem],
    searchMetricsMVar :: Metrics.SearchMetricsMVar
  }

data EstimateItem = EstimateItem
  { vehicleVariant :: Variant.Variant,
    distanceToPickup :: Meters,
    baseFare :: Money
  }

data TransporterInfo = TransporterInfo
  { shortId :: ShortId DM.Merchant,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

handler :: Id DM.Merchant -> DSearchReq -> Flow DSearchRes
handler merchantId sReq = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless org.enabled $ throwError AgencyDisabled
  searchMetricsMVar <- Metrics.startSearchMetrics org.name
  fromLocation <- buildSearchReqLocation sReq.pickupLocation
  toLocation <- buildSearchReqLocation sReq.dropLocation
  let pickupLatLong = Maps.getCoordinates fromLocation
  let mbDropoffLatLong = Just $ Maps.getCoordinates toLocation
  unlessM (rideServiceable org.geofencingConfig QGeometry.someGeometriesContain pickupLatLong mbDropoffLatLong) $
    throwError RideNotServiceable

  distRes <-
    Maps.getDistance merchantId $
      Maps.GetDistanceReq
        { origin = fromLocation,
          destination = toLocation,
          travelMode = Just Maps.CAR
        }
  let distance = distRes.distance
  logDebug $ "distance: " <> show distance

  allFarePolicies <- FarePolicyS.findAllByMerchantId org.id
  let farePolicies = filter (checkTripConstraints distance) allFarePolicies

  estimates <-
    if null farePolicies
      then do
        logDebug "Trip doesnot match any fare policy constraints."
        return []
      else do
        mdriverPoolLimitForRandomize <- asks (.driverPoolLimit)
        let shouldFilterByActualDistance = isNothing mdriverPoolLimitForRandomize
        driverPool' <- calculateDriverPool Nothing fromLocation org.id True shouldFilterByActualDistance

        driverPool <-
          maybe
            (return driverPool')
            (randomizeAndLimitSelection driverPool')
            mdriverPoolLimitForRandomize

        logDebug $ "Search handler: driver pool " <> show driverPool

        let getVariant x = x.origin.vehicle.variant
            listOfProtoQuotes = nubBy ((==) `on` getVariant) driverPool
            filteredProtoQuotes = zipMatched farePolicies listOfProtoQuotes

        estimates <- mapM (mkEstimate org sReq.pickupTime distance) filteredProtoQuotes
        logDebug $ "bap uri: " <> show sReq.bapUri
        return estimates

  buildSearchRes org fromLocation toLocation estimates searchMetricsMVar
  where
    checkTripConstraints tripDistance fp =
      let cond1 = (<= tripDistance) <$> fp.minAllowedTripDistance
          cond2 = (>= tripDistance) <$> fp.maxAllowedTripDistance
       in and $ catMaybes [cond1, cond2]

    randomizeAndLimitSelection driverPool limit = do
      let poolLen = length driverPool
          startIdx = 0
          endIdx = poolLen - 1
      randomNumList <- getRandomNumberList startIdx endIdx limit
      return $ fmap (driverPool !!) randomNumList

-- Generate `count` number of random numbers with bounds `start` and `end`
getRandomNumberList :: (L.MonadFlow m) => Int -> Int -> Int -> m [Int]
getRandomNumberList start end count = do
  n <- round <$> L.runIO getPOSIXTime
  let pureGen = mkStdGen n
  return $ toList $ nextNumber pureGen Set.empty
  where
    nextNumber :: RandomGen g => g -> Set.Set Int -> Set.Set Int
    nextNumber gen acc =
      if Set.size acc == min (end - start + 1) count
        then acc
        else
          let (n, gen') = randomR (start, end) gen
           in nextNumber gen' (Set.union (Set.singleton n) acc)

type DriverMetaData = Maps.GetDistanceResp QP.DriverPoolResult Maps.LatLong

zipMatched :: [FarePolicy] -> [DriverMetaData] -> [(FarePolicy, DriverMetaData)]
zipMatched farePolicies driverPool =
  mapMaybe match farePolicies
  where
    match :: FarePolicy -> Maybe (FarePolicy, DriverMetaData)
    match farePolicy =
      let fpVehicleVariant = farePolicy.vehicleVariant
          driverPoolVariants = map (\d -> d.origin.vehicle.variant) driverPool
          midx = elemIndex fpVehicleVariant driverPoolVariants
       in fmap (\idx -> (farePolicy, driverPool !! idx)) midx

mkEstimate ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) =>
  DM.Merchant ->
  UTCTime ->
  Meters ->
  (FarePolicy, Maps.GetDistanceResp DriverPoolResult a) ->
  m EstimateItem
mkEstimate org startTime dist (farePolicy, driverMetadata) = do
  fareParams <- calculateFare org.id farePolicy dist startTime Nothing
  let baseFare = fareSum fareParams
  logDebug $ "baseFare: " <> show baseFare
  logDebug $ "distanceToPickup: " <> show driverMetadata.distance
  pure
    EstimateItem
      { vehicleVariant = driverMetadata.origin.vehicle.variant,
        distanceToPickup = driverMetadata.distance,
        baseFare
      }

buildSearchReqLocation :: (MonadGuid m, MonadTime m) => DLoc.SearchReqLocationAPIEntity -> m DLoc.SearchReqLocation
buildSearchReqLocation DLoc.SearchReqLocationAPIEntity {..} = do
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}

buildSearchRes ::
  (MonadTime m) =>
  DM.Merchant ->
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  [EstimateItem] ->
  Metrics.SearchMetricsMVar ->
  m DSearchRes
buildSearchRes org fromLocation toLocation estimateList searchMetricsMVar = do
  now <- getCurrentTime
  pure $
    DSearchRes
      { provider = org,
        now,
        fromLocation,
        toLocation,
        estimateList,
        searchMetricsMVar
      }
