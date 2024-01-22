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
    readCsvAndGetEfficientRouteInfo,
  )
where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import Domain.Action.UI.HotSpot
import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Action.UI.Serviceability as Serviceability
import Domain.Types.HotSpot hiding (updatedAt)
import Domain.Types.HotSpotConfig
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.NextBillionData as DNB
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Domain.Types.SavedReqLocation
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps
import qualified Kernel.External.Maps as MapsK
import qualified Kernel.External.Maps.Interface.NextBillion as NextBillion
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.HotSpotConfig as QHotSpotConfig
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.MerchantConfig as QMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.SavedLocation as CSavedLocation
import qualified Storage.Queries.NextBillionData as QNB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person.PersonDisability as PD
import qualified Storage.Queries.SearchRequest as QSearchRequest
import System.Directory
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import Tools.Metrics
import qualified Tools.Metrics as Metrics
import qualified Tools.Search as Search

data OneWaySearchReq = OneWaySearchReq
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation,
    isSourceManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    isReallocationEnabled :: Maybe Bool
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
    city :: City,
    customerLanguage :: Maybe Maps.Language,
    disabilityTag :: Maybe Text,
    device :: Maybe Text,
    shortestRouteInfo :: Maybe Maps.RouteInfo,
    phoneNumber :: Maybe Text,
    isReallocationEnabled :: Maybe Bool
  }

hotSpotUpdate ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Maybe SavedReqLocation ->
  OneWaySearchReq ->
  m ()
hotSpotUpdate merchantId mbFavourite req = case mbFavourite of
  Just SavedReqLocation {..} ->
    frequencyUpdator merchantId req.origin.gps (Just req.origin.address) (bool NonManualSaved ManualSaved (isMoved == Just True))
  Nothing ->
    frequencyUpdator merchantId req.origin.gps (Just req.origin.address) (bool NonManualPickup ManualPickup (req.isSourceManuallyMoved == Just True))

updateForSpecialLocation ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    EventStreamFlow m r
  ) =>
  Id Merchant ->
  OneWaySearchReq ->
  m ()
updateForSpecialLocation merchantId req = do
  case req.isSpecialLocation of
    Just isSpecialLocation -> do
      when isSpecialLocation $ frequencyUpdator merchantId req.origin.gps (Just req.origin.address) SpecialLocation
    Nothing -> do
      specialLocationBody <- QSpecialLocation.findSpecialLocationByLatLong req.origin.gps
      case specialLocationBody of
        Just _ -> frequencyUpdator merchantId req.origin.gps (Just req.origin.address) SpecialLocation
        Nothing -> return ()

oneWaySearch ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    EsqDBFlow m r,
    HasBAPMetrics m r,
    MonadFlow m,
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
  phoneNumber <- mapM decrypt person.mobileNumber
  tag <- case person.hasDisability of
    Just True -> B.runInReplica $ fmap (.tag) <$> PD.findByPersonId personId
    _ -> return Nothing
  merchant <- QMerc.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  mbFavourite <- CSavedLocation.findByLatLonAndRiderId personId req.origin.gps
  HotSpotConfig {..} <- QHotSpotConfig.findConfigByMerchantId merchant.id >>= fromMaybeM (InternalError "config not found for merchant")

  let sourceLatLong = req.origin.gps
  let destinationLatLong = req.destination.gps
  originCity <- validateServiceability sourceLatLong destinationLatLong person <&> fromMaybe merchant.defaultCity <$> (.city)
  -- merchant operating city of search-request-origin-location

  when (shouldSaveSearchHotSpot && shouldTakeHotSpot) do
    fork "ride search geohash frequencyUpdater" $ do
      _ <- hotSpotUpdate person.merchantId mbFavourite req
      updateForSpecialLocation person.merchantId req

  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id originCity
      >>= fromMaybeM
        ( MerchantOperatingCityNotFound $
            "merchantId: " <> merchant.id.getId <> " ,city: " <> show originCity
        )
  let request =
        Maps.GetRoutesReq
          { waypoints = [sourceLatLong, destinationLatLong],
            calcPoints = True,
            mode = Just Maps.CAR
          }
  routeResponse <- Maps.getRoutes person.id person.merchantId (Just merchantOperatingCity.id) request
  let durationWeightage = 100 - merchant.distanceWeightage
  let shortestRouteInfo = getEfficientRouteInfo routeResponse merchant.distanceWeightage durationWeightage
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
      merchantOperatingCity
      (Just toLocation)
      (metersToHighPrecMeters <$> longestRouteDistance)
      (metersToHighPrecMeters <$> shortestRouteDistance)
      now
      bundleVersion
      clientVersion
      device
      tag
      shortestRouteDuration
  Metrics.incrementSearchRequestCount merchant.name

  fork "calling next billion directions api" $ do
    nextBillionConfigs <- QMSC.findByMerchantIdAndService person.merchantId (DMSC.MapsService MapsK.NextBillion) >>= fromMaybeM (MerchantServiceConfigNotFound person.merchantId.getId "Maps" "NextBillion")
    case nextBillionConfigs.serviceConfig of
      DMSC.MapsServiceConfig mapsCfg -> do
        case mapsCfg of
          MapsK.NextBillionConfig msc -> do
            nextBillionrouteResponse <- NextBillion.getRoutes msc request
            logInfo $ "NextBillion route response: " <> show nextBillionrouteResponse
            let routeData =
                  DNB.NextBillionData
                    { routes = map show nextBillionrouteResponse,
                      searchRequestId = searchRequest.id,
                      merchantId = Just merchant.id,
                      merchantOperatingCityId = Just merchantOperatingCity.id,
                      createdAt = now,
                      updatedAt = now
                    }
            QNB.create routeData
          _ -> logInfo "No NextBillion config"
      _ -> logInfo "NextBillion route not found"
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  triggerSearchEvent SearchEventData {searchRequest = searchRequest}
  _ <- QSearchRequest.createDSReq searchRequest
  _ <- QPFS.updateStatus person.id DPFS.SEARCHING {requestId = searchRequest.id, validTill = searchRequest.validTill}
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
            city = originCity,
            disabilityTag = tag,
            device,
            shortestRouteInfo,
            isReallocationEnabled = req.isReallocationEnabled,
            ..
          }
  fork "updating search counters" $ do
    merchantConfigs <- QMC.findAllByMerchantOperatingCityId person.merchantOperatingCityId
    SMC.updateSearchFraudCounters personId merchantConfigs
    mFraudDetected <- SMC.anyFraudDetected personId merchantOperatingCity.id merchantConfigs
    whenJust mFraudDetected $ \mc -> SMC.blockCustomer personId (Just mc.id)
  return dSearchRes
  where
    validateServiceability origin destination person' = do
      originServiceability <- Serviceability.checkServiceabilityAndGetCity (.origin) (person'.id, person'.merchantId) origin False
      destinationServiceability <- Serviceability.checkServiceabilityAndGetCity (.destination) (person'.id, person'.merchantId) destination False
      if originServiceability.serviceable && destinationServiceability.serviceable && originServiceability.city == destinationServiceability.city
        then pure originServiceability
        else throwError RideNotServiceable

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

getEfficientRouteInfo :: [Maps.RouteInfo] -> Int -> Int -> Maybe Maps.RouteInfo
getEfficientRouteInfo [] _ _ = Nothing
getEfficientRouteInfo routeInfos distanceWeight durationWeight = do
  let minD = Search.minDistance routeInfos
      minDur = Search.minDuration routeInfos
      normalizedInfos = Search.normalizeArr (Just minD) (Just minDur) routeInfos
      resultInfoIdx = Search.findMaxWeightedInfoIdx (fromIntegral distanceWeight) (fromIntegral durationWeight) normalizedInfos
  if resultInfoIdx < length routeInfos
    then Just (routeInfos !! resultInfoIdx)
    else Nothing

data NextBillionCsvRow = NextBillionCsvRow
  { routes :: Text,
    searchRequestId :: Text,
    routeOne :: Text,
    distanceOne :: Text,
    durationOne :: Text,
    routeTwo :: Text,
    distanceTwo :: Text,
    durationTwo :: Text,
    routeThree :: Text,
    distanceThree :: Text,
    durationThree :: Text,
    efficientRoute :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance FromNamedRecord NextBillionCsvRow where
  parseNamedRecord r =
    NextBillionCsvRow
      <$> r .: "routes"
      <*> r .: "search_request_id"
      <*> r .: "route_one"
      <*> r .: "distance_one"
      <*> r .: "duration_one"
      <*> r .: "route_two"
      <*> r .: "distance_two"
      <*> r .: "duration_two"
      <*> r .: "route_three"
      <*> r .: "distance_three"
      <*> r .: "duration_three"
      <*> r .: "efficient_route"

instance ToNamedRecord NextBillionCsvRow where
  toNamedRecord row =
    namedRecord
      [ "routes" .= routes row,
        "search_request_id" .= searchRequestId row,
        "route_one" .= routeOne row,
        "distance_one" .= distanceOne row,
        "duration_one" .= durationOne row,
        "route_two" .= routeTwo row,
        "distance_two" .= distanceTwo row,
        "duration_two" .= durationTwo row,
        "route_three" .= routeThree row,
        "distance_three" .= distanceThree row,
        "duration_three" .= durationThree row,
        "efficient_route" .= efficientRoute row
      ]

replaceAll :: T.Text -> T.Text -> T.Text -> T.Text
replaceAll target replacement input =
  if T.isInfixOf target input
    then replaceAll target replacement (T.replace target replacement input)
    else input

readCsvAndGetEfficientRouteInfo :: IO ()
readCsvAndGetEfficientRouteInfo = do
  currentDirectory <- getCurrentDirectory
  putStrLn $ "Current Working Directory: " ++ currentDirectory
  csvData <- BL.readFile "data.csv"
  case decodeByName @NextBillionCsvRow csvData of
    Left err -> putStrLn $ "Error: " ++ err
    Right (csvHeader, csvResults) -> do
      -- forM_ csvResults $ \result -> do
      --   putStrLn (replaceAll "\",\"" "," $ replaceAll "{\"" "[" $ replaceAll "\"}" "]" $ replaceAll "\"\"" "" result.routes)
      --   let routesInfo :: [Maps.RouteInfo] = read $ T.unpack (replaceAll "\",\"" "," $ replaceAll "{\"" "[" $ replaceAll "\"}" "]" $ replaceAll "\"\"" "" result.routes)
      --   putStrLn $ "[Maps.RouteInfo]: " ++ show routesInfo
      let outputRows = map processRow (V.toList csvResults)
      BL.writeFile "output.csv" $ encodeByName csvHeader outputRows
      putStrLn ("Output CSV file written successfully." :: Text)

data Route = Route
  { route :: Text,
    distance :: Text,
    duration :: Text
  }

-- SELECT routes, search_request_id, '{}' as route_one, '{}' as distance_one, '{}' as duration_one, '{}' as route_two, '{}' as distance_two, '{}' as duration_two, '{}' as route_three, '{}' as distance_three, '{}' as duration_three, '{}' as efficient_route FROM atlas_app.next_billion_data LIMIT 1;
-- readMaybe "[RouteInfo {duration = Just 1244, distance = Just 5871, boundingBox = Nothing, snappedWaypoints = [], points = [LatLong {lat = 12.95003, lon = 77.60797}]}]" :: Maybe [Maps.RouteInfo]
processRow :: NextBillionCsvRow -> NextBillionCsvRow
processRow result =
  let routesInfo :: [Maps.RouteInfo] = read $ T.unpack (replaceAll "\",\"" "," $ replaceAll "{\"" "[" $ replaceAll "\"}" "]" $ replaceAll "\"\"" "" result.routes)
      distanceWeightage = 70
      durationWeightage = 100 - distanceWeightage
      efficientRoute =
        fromMaybe "{}" (encodeToText <$> getEfficientRouteInfo routesInfo distanceWeightage durationWeightage)
      (routeOne, routeTwo, routeThree) =
        case routesInfo of
          [routeOne', routeTwo', routeThree'] -> (Route (encodeToText routeOne') (maybe "" show routeOne'.distance) (maybe "" show routeOne'.duration), Route (encodeToText routeTwo') (maybe "" show routeTwo'.distance) (maybe "" show routeTwo'.duration), Route (encodeToText routeThree') (maybe "" show routeThree'.distance) (maybe "" show routeThree'.duration))
          [routeOne', routeTwo'] -> (Route (encodeToText routeOne') (maybe "" show routeOne'.distance) (maybe "" show routeOne'.duration), Route (encodeToText routeTwo') (maybe "" show routeTwo'.distance) (maybe "" show routeTwo'.duration), Route "{}" "" "")
          [routeOne'] -> (Route (encodeToText routeOne') (maybe "" show routeOne'.distance) (maybe "" show routeOne'.duration), Route "{}" "" "", Route "{}" "" "")
          _ -> (Route "{}" "" "", Route "{}" "" "", Route "{}" "" "")
   in NextBillionCsvRow
        { routes = result.routes,
          searchRequestId = result.searchRequestId,
          routeOne = routeOne.route,
          distanceOne = routeOne.distance,
          durationOne = routeOne.duration,
          routeTwo = routeTwo.route,
          distanceTwo = routeTwo.distance,
          durationTwo = routeTwo.duration,
          routeThree = routeThree.route,
          distanceThree = routeThree.distance,
          durationThree = routeThree.duration,
          efficientRoute = efficientRoute
        }
