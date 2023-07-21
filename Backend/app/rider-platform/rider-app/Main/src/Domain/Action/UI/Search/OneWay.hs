{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLists #-}

module Domain.Action.UI.Search.OneWay
  ( OneWaySearchReq (..),
    OneWaySearchRes (..),
    DSearch.SearchReqLocation (..),
    oneWaySearch,
  )
where

import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.SearchRequest as DSearchReq
import Kernel.Prelude
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (CacheFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.DirectionsCache as SDC
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.CachedQueries.MerchantConfig as QMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import Storage.Queries.Geometry
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import Tools.Metrics
import qualified Tools.Metrics as Metrics

data OneWaySearchReq = OneWaySearchReq
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OneWaySearchRes = OneWaySearchRes
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation,
    searchId :: Id DSearchReq.SearchRequest,
    now :: UTCTime,
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime,
    merchant :: DM.Merchant,
    customerLanguage :: Maybe Maps.Language,
    device :: Maybe Text,
    shortestRouteInfo :: Maybe Maps.RouteInfo
  }

oneWaySearch ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  Id Person.Person ->
  OneWaySearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  m OneWaySearchRes
oneWaySearch personId req bundleVersion clientVersion device = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <- QMerc.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)

  validateServiceability merchant.geofencingConfig

  let sourceLatlong = req.origin.gps
  let destinationLatLong = req.destination.gps
  let request =
        Maps.GetRoutesReq
          { waypoints = [sourceLatlong, destinationLatLong],
            calcPoints = True,
            mode = Just Maps.CAR
          }
  routeResponse <- SDC.getRoutes person.merchantId request
  let shortestRouteInfo = getRouteInfoWithShortestDuration routeResponse
  let longestRouteDistance = (.distance) =<< getLongestRouteDistance routeResponse
  let shortestRouteDistance = (.distance) =<< shortestRouteInfo
  let shortestRouteDuration = (.duration) =<< shortestRouteInfo

  fromLocation <- DSearch.buildSearchReqLoc req.origin
  toLocation <- DSearch.buildSearchReqLoc req.destination
  now <- getCurrentTime
  searchRequest <-
    DSearch.buildSearchRequest
      person
      fromLocation
      (Just toLocation)
      (metersToHighPrecMeters <$> longestRouteDistance)
      (metersToHighPrecMeters <$> shortestRouteDistance)
      now
      bundleVersion
      clientVersion
      device
      shortestRouteDuration
  Metrics.incrementSearchRequestCount merchant.name
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  triggerSearchEvent SearchEventData {searchRequest = searchRequest}
  DB.runTransaction $ do
    QSearchRequest.create searchRequest
    QPFS.updateStatus person.id DPFS.SEARCHING {requestId = searchRequest.id, validTill = searchRequest.validTill}
  QPFS.clearCache person.id
  let dSearchRes =
        OneWaySearchRes
          { origin = req.origin,
            destination = req.destination,
            searchId = searchRequest.id,
            now = now,
            gatewayUrl = merchant.gatewayUrl,
            searchRequestExpiry = searchRequest.validTill,
            customerLanguage = searchRequest.language,
            device,
            shortestRouteInfo,
            ..
          }
  fork "updating search counters" $ do
    merchantConfigs <- QMC.findAllByMerchantId person.merchantId
    SMC.updateSearchFraudCounters personId merchantConfigs
    mFraudDetected <- SMC.anyFraudDetected personId person.merchantId merchantConfigs
    whenJust mFraudDetected $ \mc -> SMC.blockCustomer personId (Just mc.id)
  return dSearchRes
  where
    validateServiceability geoConfig =
      unlessM (rideServiceable geoConfig someGeometriesContain req.origin.gps (Just req.destination.gps)) $
        throwError RideNotServiceable

getLongestRouteDistance :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getLongestRouteDistance [] = Nothing
getLongestRouteDistance (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteresult <- getLongestRouteDistance routeInfoArray
      Just $ comparator' routeInfo restRouteresult
  where
    comparator' route1 route2 =
      if route1.distance > route2.distance
        then route1
        else route2

getRouteInfoWithShortestDuration :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getRouteInfoWithShortestDuration (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteresult <- getRouteInfoWithShortestDuration routeInfoArray
      Just $ comparator routeInfo restRouteresult
getRouteInfoWithShortestDuration [] = Nothing

comparator :: Maps.RouteInfo -> Maps.RouteInfo -> Maps.RouteInfo
comparator route1 route2 =
  if route1.duration < route2.duration
    then route1
    else route2
