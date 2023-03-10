{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Search
  ( DSearchReq (..),
    DSearchRes (..),
    handler,
  )
where

import qualified Data.Map as M
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.Prelude
import Kernel.Serviceability
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.DriverPool hiding (lat, lon)
import SharedLogic.Estimate (EstimateItem, buildEstimate)
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Geometry as QGeometry
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics.ARDUBPPMetrics as Metrics

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: LatLong,
    pickupTime :: UTCTime,
    dropLocation :: LatLong,
    routeInfo :: Maybe Maps.RouteInfo
  }

data DSearchRes = DSearchRes
  { provider :: DM.Merchant,
    fromLocation :: LatLong,
    toLocation :: LatLong,
    now :: UTCTime,
    estimateList :: [EstimateItem],
    searchMetricsMVar :: Metrics.SearchMetricsMVar
  }

data DistanceAndDuration = DistanceAndDuration
  { distance :: Meters,
    duration :: Seconds
  }

getDistanceAndDuration :: Id DM.Merchant -> LatLong -> LatLong -> Maybe Maps.RouteInfo -> Flow DistanceAndDuration
getDistanceAndDuration merchantId fromLocation toLocation routeInfo = case routeInfo of
  Just (Maps.RouteInfo (Just duration) (Just distance) _ _ _) -> return $ DistanceAndDuration {distance, duration}
  _ -> getMapsDistance
  where
    getMapsDistance = do
      response <-
        Maps.getDistance merchantId $
          Maps.GetDistanceReq
            { origin = fromLocation,
              destination = toLocation,
              travelMode = Just Maps.CAR
            }
      return DistanceAndDuration {distance = response.distance, duration = response.duration}

handler :: Id DM.Merchant -> DSearchReq -> Flow DSearchRes
handler merchantId sReq = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless org.enabled $ throwError AgencyDisabled
  searchMetricsMVar <- Metrics.startSearchMetrics org.name
  let fromLocation = sReq.pickupLocation
      toLocation = sReq.dropLocation
  unlessM (rideServiceable org.geofencingConfig QGeometry.someGeometriesContain fromLocation (Just toLocation)) $
    throwError RideNotServiceable
  result <- getDistanceAndDuration merchantId fromLocation toLocation sReq.routeInfo
  CD.cacheDistance sReq.transactionId (result.distance, result.duration)
  logDebug $ "distance: " <> show result.distance

  allFarePolicies <- FarePolicyS.findAllByMerchantId org.id (Just result.distance)
  let farePolicies = filter (checkTripConstraints result.distance) allFarePolicies

  estimates <-
    if null farePolicies
      then do
        logDebug "Trip doesnot match any fare policy constraints."
        return []
      else do
        driverPoolCfg <- getDriverPoolConfig result.distance
        driverPool <- calculateDriverPool Estimate driverPoolCfg Nothing fromLocation org.id True Nothing

        logDebug $ "Search handler: driver pool " <> show driverPool

        let listOfProtoQuotes = foldl (\m dpr -> M.insertWith (<>) dpr.variant (pure dpr) m) mempty driverPool
            filteredProtoQuotes = zipMatched farePolicies listOfProtoQuotes
        estimates <- mapM (buildEstimate org sReq.pickupTime result.distance) filteredProtoQuotes
        logDebug $ "bap uri: " <> show sReq.bapUri
        return estimates

  buildSearchRes org fromLocation toLocation estimates searchMetricsMVar
  where
    checkTripConstraints tripDistance fp =
      let cond1 = (<= tripDistance) <$> fp.minAllowedTripDistance
          cond2 = (>= tripDistance) <$> fp.maxAllowedTripDistance
       in and $ catMaybes [cond1, cond2]

    zipMatched farePolicies driverPools = do
      farePolicy <- farePolicies
      let driverPool = M.lookup farePolicy.vehicleVariant driverPools
      case driverPool of
        Nothing -> mempty
        Just dp -> return (farePolicy, dp)

buildSearchRes ::
  (MonadTime m) =>
  DM.Merchant ->
  LatLong ->
  LatLong ->
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
