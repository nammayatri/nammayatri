module SharedLogic.TransitLive
  ( listLiveVehicles,
    getLiveVehicle,
  )
where

import API.Types.Dashboard.AppManagement.TransitOperatorV2Live (LiveStopEta (..), LiveTrip (..), LiveTripStop (..), LiveVehicle (..), LiveVehicleDetail (..), LiveVehicleList (..))
import Control.Applicative ((<|>))
import Data.Aeson (withObject, (.!=), (.:), (.:?))
import qualified Data.Char as C
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, LocalTime (..), TimeZone, addDays, defaultTimeLocale, localDay, localTimeToUTC, midnight, minutesToTimeZone, parseTimeM, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Utils (mapConcurrently)
import Environment (Flow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.Masters.API as MastersAPI
import qualified "bms-backend" Lib.BmsBackend.Transit.Masters.Types as Masters
import qualified "bms-backend" Lib.BmsBackend.Transit.V2.API as V2API
import qualified "bms-backend" Lib.BmsBackend.Transit.V2.Types as V2
import qualified SharedLogic.External.Nandi.Flow as Nandi
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import SharedLogic.TransitOperatorV2 (TransitCtx (..), TransitFeedCtx (..), tms)
import qualified Storage.CachedQueries.Merchant.MultiModalBus as MMB
import qualified Text.Read as TR

data RouteMeta = RouteMeta
  { rmNumber :: Maybe Text,
    rmState :: Maybe Text,
    rmMatchedAt :: Maybe Int,
    rmStopSeq :: Maybe Int
  }

instance FromJSON RouteMeta where
  parseJSON = withObject "RouteMeta" $ \v ->
    RouteMeta
      <$> v .:? "route_number"
      <*> v .:? "route_state"
      <*> (fmap (floor @Double) <$> v .:? "timestamp")
      <*> (fmap (floor @Double) <$> v .:? "stop_sequence")

data BusMeta = BusMeta
  { mLat :: Double,
    mLon :: Double,
    mPingAt :: Int,
    mDeviceSpeed :: Maybe Double,
    mHaversineSpeed :: Maybe Double,
    mDeviceId :: Maybe Text,
    mBearing :: Maybe Double,
    mRoutes :: M.Map Text RouteMeta
  }

instance FromJSON BusMeta where
  parseJSON = withObject "BusMeta" $ \v ->
    BusMeta
      <$> v .: "latitude"
      <*> v .: "longitude"
      <*> (floor @Double <$> v .: "timestamp")
      <*> v .:? "device_speed"
      <*> v .:? "haversine_speed"
      <*> v .:? "device_id"
      <*> v .:? "bearing"
      <*> v .:? "routes_info" .!= M.empty

data EtaEntry = EtaEntry
  { eStopId :: Text,
    eSeq :: Int,
    eStopName :: Maybe Text,
    eLat :: Maybe Double,
    eLon :: Maybe Double,
    eArrival :: Int
  }

instance FromJSON EtaEntry where
  parseJSON = withObject "EtaEntry" $ \v ->
    EtaEntry
      <$> v .: "stop_id"
      <*> (floor @Double <$> v .: "stop_seq")
      <*> v .:? "stop_name"
      <*> v .:? "stop_lat"
      <*> v .:? "stop_lon"
      <*> (floor @Double <$> v .: "arrival_time")

data RouteLive = RouteLive
  { rlTripId :: Maybe Text,
    rlUpcoming :: Bool,
    rlEtas :: [EtaEntry]
  }

instance FromJSON RouteLive where
  parseJSON = withObject "RouteLive" $ \v ->
    RouteLive
      <$> v .:? "current_trip_id"
      <*> v .:? "is_upcoming_trip" .!= False
      <*> v .:? "eta_data" .!= []

data RunInfo = RunInfo
  { riWaybill :: Text,
    riSchedule :: Maybe Text,
    riSource :: Text,
    riDriver :: Maybe Text,
    riConductor :: Maybe Text,
    riTrips :: [LiveTrip]
  }

maxRadiusMeters :: Double
maxRadiusMeters = 5000

enrichLimit :: Int
enrichLimit = 500

vehicleCacheSeconds :: Int
vehicleCacheSeconds = 60

ist :: TimeZone
ist = minutesToTimeZone 330

redisPrefix :: DIBC.IntegratedBPPConfig -> Maybe Text
redisPrefix bppConfig = case bppConfig.providerConfig of
  DIBC.ONDC config -> config.redisPrefix
  DIBC.DIRECT config -> config.redisPrefix
  _ -> Nothing

prefixed :: Maybe Text -> Text -> Text
prefixed (Just p) suffix | p /= "" = p <> ":" <> suffix
prefixed _ suffix = suffix

metadataKey :: Maybe Text -> Text
metadataKey p = prefixed p "bus_metadata_v2"

normVno :: Text -> Text
normVno = T.toUpper . T.filter C.isAlphaNum

isAdmin :: TransitCtx -> Bool
isAdmin ctx = ctx.caller.orgId == "*"

pingTime :: BusMeta -> UTCTime
pingTime m = posixSecondsToUTCTime (fromIntegral m.mPingAt)

safeHmGet :: FromJSON a => Text -> [Text] -> Flow [Maybe a]
safeHmGet key fields =
  withTryCatch "transitLive:hmGet" (Hedis.runInMultiCloudLTSRedisForMaybeListFromReplica $ Hedis.hmGet key fields) >>= \case
    Right values -> pure values
    Left err -> do
      logError $ "transitLive hmGet failed for " <> key <> ": " <> show err
      pure (map (const Nothing) fields)

orgVehicles :: TransitCtx -> Flow [Masters.Vehicle]
orgVehicles ctx = do
  let key = "transitLive:tmsVehicles:" <> ctx.gtfsId <> ":" <> ctx.caller.orgId
  Hedis.safeGet key >>= \case
    Just vehicles -> pure vehicles
    Nothing -> do
      vehicles <- fetchPage 0 []
      Hedis.setExp key vehicles vehicleCacheSeconds
      pure vehicles
  where
    pageSize = 1000
    fetchPage :: Int -> [Masters.Vehicle] -> Flow [Masters.Vehicle]
    fetchPage offset acc = do
      page <- tms ctx "listVehicles" MastersAPI.listVehiclesAPI (MastersAPI.listVehicles ctx.gtfsId Nothing Nothing Nothing Nothing Nothing Nothing (Just pageSize) (Just offset))
      let acc' = acc <> page.items
      if null page.items || length acc' >= page.total || offset >= 50 * pageSize
        then pure acc'
        else fetchPage (offset + pageSize) acc'

vehicleIndex :: TransitCtx -> Flow (M.Map Text Masters.Vehicle)
vehicleIndex ctx = do
  vehicles <-
    if isAdmin ctx
      then
        withTryCatch "transitLive:orgVehicles" (orgVehicles ctx) >>= \case
          Right vs -> pure vs
          Left err -> do
            logError $ "transitLive TMS vehicles unavailable: " <> show err
            pure []
      else orgVehicles ctx
  pure $ M.fromList [(normVno vno, vehicle) | vehicle <- vehicles, Just vno <- [vehicle.vehicleNo]]

stateRank :: Maybe Text -> Int
stateRank = \case
  Just "ConfirmedHigh" -> 0
  Just "ConfirmedMed" -> 1
  Just "Alternate" -> 2
  Just "Schedule" -> 3
  _ -> 4

primaryRoute :: Maybe Text -> BusMeta -> Maybe Text
primaryRoute (Just routeId) m | M.member routeId m.mRoutes = Just routeId
primaryRoute _ m =
  fst <$> listToMaybe (sortOn (\(_, rm) -> (stateRank rm.rmState, Down rm.rmMatchedAt)) (M.toList m.mRoutes))

distanceMeters :: Double -> Double -> Double -> Double -> Double
distanceMeters lat1 lon1 lat2 lon2 =
  let toRad d = d * pi / 180
      dLat = toRad (lat2 - lat1)
      dLon = toRad (lon2 - lon1)
      a = sin (dLat / 2) ^ (2 :: Int) + cos (toRad lat1) * cos (toRad lat2) * sin (dLon / 2) ^ (2 :: Int)
   in 2 * 6371000 * atan2 (sqrt a) (sqrt (1 - a))

routeLives :: Maybe Text -> [(Text, Text)] -> Flow (M.Map (Text, Text) RouteLive)
routeLives p pairs = do
  let byRoute = M.toList $ M.fromListWith (<>) [(routeId, [vno]) | (vno, routeId) <- pairs]
  results <- flip mapConcurrently byRoute $ \(routeId, vnos) -> do
    current <- safeHmGet (prefixed p ("route:" <> routeId)) vnos
    let missing = [vno | (vno, Nothing) <- zip vnos current]
    upcoming <- safeHmGet (prefixed p ("route:upcoming:" <> routeId)) missing
    pure $
      [((vno, routeId), live) | (vno, Just live) <- zip vnos current]
        <> [((vno, routeId), live {rlUpcoming = True}) | (vno, Just live) <- zip missing upcoming]
  pure $ M.fromList (concat results)

mkLiveVehicle :: UTCTime -> Maybe Masters.Vehicle -> Text -> BusMeta -> Maybe Text -> Maybe RouteLive -> LiveVehicle
mkLiveVehicle now mbVehicle vno m mbRouteId mbLive =
  let lastPingAt = pingTime m
      mbRoute = mbRouteId >>= (`M.lookup` m.mRoutes)
   in LiveVehicle
        { vehicleNo = vno,
          deviceId = m.mDeviceId,
          lat = m.mLat,
          lon = m.mLon,
          speedKmph = m.mDeviceSpeed <|> ((* 3.6) <$> m.mHaversineSpeed),
          bearing = m.mBearing,
          lastPingAt,
          ageSeconds = max 0 (floor (diffUTCTime now lastPingAt)),
          routeId = mbRouteId,
          routeNumber = mbRoute >>= (.rmNumber),
          tripId = mbLive >>= (.rlTripId),
          routeState = mbRoute >>= (.rmState),
          isUpcoming = maybe False (.rlUpcoming) mbLive,
          depotId = mbVehicle >>= (.depotId),
          depotName = mbVehicle >>= (.depotName),
          serviceType = mbVehicle >>= (.serviceTypeName)
        }

listLiveVehicles :: TransitFeedCtx -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Double -> Flow LiveVehicleList
listLiveVehicles feed mbDepotId mbRouteId mbLat mbLon mbRadius = do
  now <- getCurrentTime
  let p = redisPrefix feed.bppConfig
      radius = max 0 (min maxRadiusMeters (fromMaybe maxRadiusMeters mbRadius))
      mbCenter = (,) <$> mbLat <*> mbLon
  metas <- case mbCenter of
    Just (la, lo) -> do
      members <- map TE.decodeUtf8 <$> Hedis.runInMultiCloudLTSRedisForListFromReplica (Hedis.geoSearch (prefixed p "bus_locations") (Hedis.FromLonLat lo la) (Hedis.ByRadius radius "m"))
      values <- safeHmGet (metadataKey p) members
      pure [(vno, m) | (vno, Just m) <- zip members values]
    Nothing -> Hedis.runInMultiCloudLTSRedisForListFromReplica $ Hedis.hGetAll (metadataKey p)
  vehicles <- vehicleIndex feed.ctx
  let admin = isAdmin feed.ctx
      inRadius m = maybe True (\(la, lo) -> distanceMeters la lo m.mLat m.mLon <= radius) mbCenter
      keep (vno, m) = do
        let mbVehicle = M.lookup (normVno vno) vehicles
        guard (admin || isJust mbVehicle)
        guard (maybe True (\d -> (mbVehicle >>= (.depotId)) == Just d) mbDepotId)
        guard (maybe True (`M.member` m.mRoutes) mbRouteId)
        guard (inRadius m)
        pure (vno, m, mbVehicle, primaryRoute mbRouteId m)
      selected = mapMaybe keep metas
  lives <-
    if length selected <= enrichLimit
      then routeLives p [(vno, routeId) | (vno, _, _, Just routeId) <- selected]
      else pure M.empty
  pure
    LiveVehicleList
      { items = [mkLiveVehicle now mbVehicle vno m mbRoute (mbRoute >>= \r -> M.lookup (vno, r) lives) | (vno, m, mbVehicle, mbRoute) <- selected],
        asOf = now
      }

emptyDetail :: LiveVehicleDetail
emptyDetail =
  LiveVehicleDetail
    { vehicle = Nothing,
      waybillNo = Nothing,
      scheduleNo = Nothing,
      source = Nothing,
      driverToken = Nothing,
      conductorToken = Nothing,
      currentTrip = Nothing,
      upcomingTrips = [],
      completedTrips = [],
      nextStops = [],
      tripStops = [],
      routePolyline = Nothing
    }

mkStopEta :: EtaEntry -> LiveStopEta
mkStopEta e =
  LiveStopEta
    { stopId = e.eStopId,
      stopName = e.eStopName,
      stopSeq = e.eSeq,
      lat = e.eLat,
      lon = e.eLon,
      predictedArrival = posixSecondsToUTCTime (fromIntegral e.eArrival),
      scheduledArrival = Nothing,
      delaySeconds = Nothing
    }

getLiveVehicle :: TransitFeedCtx -> Text -> Flow LiveVehicleDetail
getLiveVehicle feed vno = do
  now <- getCurrentTime
  vehicles <- vehicleIndex feed.ctx
  let mbVehicle = M.lookup (normVno vno) vehicles
  if not (isAdmin feed.ctx) && isNothing mbVehicle
    then pure emptyDetail
    else do
      let p = redisPrefix feed.bppConfig
      mbMeta <- join . listToMaybe <$> safeHmGet (metadataKey p) [vno]
      let mbRouteId = mbMeta >>= primaryRoute Nothing
      mbLive <- case mbRouteId of
        Just routeId -> M.lookup (vno, routeId) <$> routeLives p [(vno, routeId)]
        Nothing -> pure Nothing
      mbRun <- v2Run feed now vno >>= maybe (v1Run feed vno (mbLive >>= (.rlTripId))) (pure . Just)
      let trips = maybe [] (.riTrips) mbRun
          withStatus statuses = filter (\t -> t.status `elem` statuses) trips
          currentTrip = listToMaybe (withStatus [activeStatus])
      (tripStops, routePolyline) <- buildTripStops feed mbRun currentTrip mbRouteId mbMeta mbLive
      pure
        LiveVehicleDetail
          { vehicle = (\m -> mkLiveVehicle now mbVehicle vno m mbRouteId mbLive) <$> mbMeta,
            waybillNo = (.riWaybill) <$> mbRun,
            scheduleNo = mbRun >>= (.riSchedule),
            source = (.riSource) <$> mbRun,
            driverToken = mbRun >>= (.riDriver),
            conductorToken = mbRun >>= (.riConductor),
            currentTrip,
            upcomingTrips = withStatus [upcomingStatus],
            completedTrips = filter (\t -> t.status `notElem` [activeStatus, upcomingStatus]) trips,
            nextStops = maybe [] (map mkStopEta . sortOn (.eSeq) . (.rlEtas)) mbLive,
            tripStops,
            routePolyline
          }

buildTripStops :: TransitFeedCtx -> Maybe RunInfo -> Maybe LiveTrip -> Maybe Text -> Maybe BusMeta -> Maybe RouteLive -> Flow ([LiveTripStop], Maybe Text)
buildTripStops feed mbRun mbCurrent mbLiveRouteId mbMeta mbLive =
  case ((.routeId) <$> mbCurrent) <|> mbLiveRouteId of
    Nothing -> pure ([], Nothing)
    Just routeId -> do
      let url = feed.gimsUrl
          gtfsId = feed.ctx.gtfsId
          onLiveRoute = mbLiveRouteId == Just routeId
          etas = if onLiveRoute then maybe [] (.rlEtas) mbLive else []
          etaMap = M.fromList [(e.eStopId, e) | e <- etas]
          watermark = if onLiveRoute then mbMeta >>= (M.lookup routeId . (.mRoutes)) >>= (.rmStopSeq) else Nothing
          minEtaSeq = listToMaybe (sort (map (.eSeq) etas))
      mapping <-
        withTryCatch "transitLive:routeStops" (Nandi.getRouteStopMappingByRouteCode url gtfsId routeId) >>= \case
          Right stops -> pure stops
          Left err -> do
            logError $ "transitLive route stops failed for " <> routeId <> ": " <> show err
            pure []
      mbRoute <- Nandi.getRouteByRouteId url gtfsId routeId
      schedule <- case (mbRun, mbCurrent) of
        (Just run, Just trip) | run.riSource == "v1" -> tripSchedule url gtfsId run.riWaybill trip.tripNumber routeId
        _ -> pure M.empty
      let base =
            if null mapping
              then [(e.eStopId, e.eStopName, e.eSeq, e.eLat, e.eLon) | e <- sortOn (.eSeq) etas]
              else [(m.stopCode, Just m.stopName, m.sequenceNum, Just m.stopPoint.lat, Just m.stopPoint.lon) | m <- sortOn (.sequenceNum) mapping]
          isPassed code sq = case watermark of
            Just w -> sq < w
            Nothing -> maybe False (\s -> sq < s && not (M.member code etaMap)) minEtaSeq
          toStop (code, name, sq, la, lo) =
            let scheduledArrival = M.lookup code schedule
                predictedArrival = posixSecondsToUTCTime . fromIntegral . (.eArrival) <$> M.lookup code etaMap
             in LiveTripStop
                  { stopId = code,
                    stopCode = Just code,
                    stopName = name,
                    stopSeq = sq,
                    lat = la,
                    lon = lo,
                    scheduledArrival,
                    predictedArrival,
                    delaySeconds = (\pr sc -> round (diffUTCTime pr sc)) <$> predictedArrival <*> scheduledArrival,
                    passed = isPassed code sq
                  }
      pure (map toStop base, mbRoute >>= (.encodedPolyline))

tripSchedule :: BaseUrl -> Text -> Text -> Int -> Text -> Flow (M.Map Text UTCTime)
tripSchedule url gtfsId waybill tripNumber routeId =
  withTryCatch "transitLive:busTripSchedule" (Nandi.getBusTripSchedule url gtfsId waybill tripNumber routeId) >>= \case
    Right details -> do
      let matching = filter (\d -> d.trip_number == Just tripNumber) details
      pure $ case listToMaybe (matching <> details) of
        Just d -> M.fromList [(e.stopCode, posixSecondsToUTCTime (fromIntegral e.arrivalTimeUnix)) | (e :: MMB.BusStopETA) <- d.eta]
        Nothing -> M.empty
    Left err -> do
      logError $ "transitLive bus trip schedule failed for " <> waybill <> ": " <> show err
      pure M.empty

activeStatus :: Text
activeStatus = "active"

upcomingStatus :: Text
upcomingStatus = "upcoming"

completedStatus :: Text
completedStatus = "completed"

tripKey :: Text -> Int -> Text
tripKey waybill n = waybill <> "-" <> show n

pickRun :: UTCTime -> [V2.DutyGroupListItem] -> Maybe V2.DutyGroupListItem
pickRun now dgs =
  listToMaybe $
    concat
      [ filter (isJust . (.runningTripNumber)) dgs,
        filter (\d -> d.isActive && d.windowStartAt <= now && now <= d.windowEndAt) dgs,
        sortOn (.windowStartAt) (filter (\d -> d.isActive && d.pendingTrips > 0 && d.windowStartAt > now) dgs),
        sortOn (Down . (.windowEndAt)) (filter (\d -> d.windowEndAt <= now) dgs)
      ]

routeNumbers :: TransitFeedCtx -> [Text] -> Flow (M.Map Text Text)
routeNumbers feed routeIds
  | null routeIds = pure M.empty
  | otherwise =
    withTryCatch "transitLive:routeNumbers" (Nandi.getRoutesByRouteIds feed.gimsUrl feed.ctx.gtfsId (M.keys (M.fromList (map (,()) routeIds)))) >>= \case
      Right routes -> pure $ M.fromList [(r.id, n) | r <- routes, Just n <- [r.shortName]]
      Left err -> do
        logError $ "transitLive route lookup failed: " <> show err
        pure M.empty

v2Run :: TransitFeedCtx -> UTCTime -> Text -> Flow (Maybe RunInfo)
v2Run feed now vno = do
  let ctx = feed.ctx
      today = localDay (utcToLocalTime ist now)
  res <- withTryCatch "transitLive:dutyGroups" $ tms ctx "listDutyGroups" V2API.listDutyGroupsAPI (V2API.listDutyGroups ctx.gtfsId (Just 20) (Just 0) Nothing (Just (addDays (-1) today)) (Just today) Nothing Nothing (Just vno) Nothing Nothing Nothing)
  case res of
    Left err -> do
      logError $ "transitLive duty groups failed for " <> vno <> ": " <> show err
      pure Nothing
    Right page -> case pickRun now (filter (not . (.deleted)) page.items) of
      Nothing -> pure Nothing
      Just dg -> do
        duties <- filter (not . (.deleted)) <$> tms ctx "listDuties" V2API.listDutiesAPI (V2API.listDuties ctx.gtfsId dg.id)
        numbers <- routeNumbers feed (map (.routeId) duties)
        let toTrip (d :: V2.Duty) =
              LiveTrip
                { tripId = Just (tripKey dg.waybillNo d.tripOrder),
                  tripNumber = d.tripNumber,
                  routeId = d.routeId,
                  routeNumber = M.lookup d.routeId numbers,
                  scheduledStart = Just d.scheduledStartAt,
                  scheduledEnd = Just d.scheduledEndAt,
                  actualStart = d.recordedStartTime,
                  actualEnd = d.recordedEndTime,
                  status = d.status
                }
        pure $
          Just
            RunInfo
              { riWaybill = dg.waybillNo,
                riSchedule = Just dg.tripGroupCode,
                riSource = "v2",
                riDriver = dg.driverTokenNumber,
                riConductor = dg.conductorTokenNumber,
                riTrips = map toTrip (sortOn (.tripOrder) duties)
              }

v1Run :: TransitFeedCtx -> Text -> Maybe Text -> Flow (Maybe RunInfo)
v1Run feed vno mbLiveTripId = do
  let url = feed.gimsUrl
      gtfsId = feed.ctx.gtfsId
      anchor = NandiTypes.GimsOperationAnchor {conductor_token = Nothing, driver_token = Nothing, vehicle_number = Just vno}
  withTryCatch "transitLive:gimsCurrentOperation" (Nandi.gimsCurrentOperation url gtfsId anchor) >>= \case
    Left err -> do
      logDebug $ "transitLive no v1 waybill for " <> vno <> ": " <> show err
      pure Nothing
    Right op -> do
      let waybill = op.waybill_no
      cursor <- Hedis.get (waybill <> ":tripnumber")
      let fromLive = mbLiveTripId >>= T.stripPrefix (waybill <> "-") >>= TR.readMaybe . T.unpack
          prevTrip = fromMaybe 0 (cursor <|> fromLive)
          req = NandiTypes.GimsCurrentTripDetailsReq {previous_trip_number = prevTrip, conductor_token = Nothing, driver_token = Nothing, vehicle_number = Just vno}
      mbDetails <-
        withTryCatch "transitLive:gimsCurrentTripDetails" (Nandi.gimsCurrentTripDetails url gtfsId req) >>= \case
          Right details -> pure (Just details)
          Left err -> do
            logError $ "transitLive current trip details failed for " <> vno <> ": " <> show err
            pure Nothing
      mbInfo <- Nandi.getVehicleInfo url gtfsId vno
      let toTrip status (t :: NandiTypes.GimsTripInfo) =
            let start = gimsTime t.duty_date t.start_time
                end = gimsTime t.duty_date t.end_time
             in LiveTrip
                  { tripId = Just (tripKey waybill t.trip_number),
                    tripNumber = t.trip_number,
                    routeId = t.route_id,
                    routeNumber = Just t.route_number,
                    scheduledStart = start,
                    scheduledEnd = (\s e -> if e < s then addUTCTime 86400 e else e) <$> start <*> end <|> end,
                    actualStart = Nothing,
                    actualEnd = Nothing,
                    status
                  }
          trips = case mbDetails of
            Just d -> map (toTrip completedStatus) d.history <> maybe [] (pure . toTrip activeStatus) d.current <> map (toTrip upcomingStatus) d.upcoming
            Nothing -> []
      pure $
        Just
          RunInfo
            { riWaybill = waybill,
              riSchedule = (.scheduleNo) <$> mbInfo,
              riSource = "v1",
              riDriver = mbDetails >>= (.driver_token),
              riConductor = mbDetails >>= (.conductor_token),
              riTrips = trips
            }

gimsTime :: Maybe Text -> Maybe Text -> Maybe UTCTime
gimsTime mbDate mbTime = do
  raw <- T.strip <$> mbTime
  guard (not (T.null raw))
  iso8601ParseM (T.unpack raw) <|> localFull raw <|> (dateWithClock =<< ((,raw) <$> (mbDate >>= parseDay)))
  where
    parseDay :: Text -> Maybe Day
    parseDay d = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack (T.take 10 (T.strip d)))
    localFull raw =
      localTimeToUTC ist
        <$> ( parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack raw)
                <|> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack raw)
            )
    dateWithClock (day, raw) = do
      parts <- traverse (TR.readMaybe . T.unpack) (T.splitOn ":" raw)
      secs <- case parts of
        [h, m] -> Just (h * 3600 + m * 60)
        [h, m, s] -> Just (h * 3600 + m * 60 + s)
        _ -> Nothing
      pure $ addUTCTime (fromIntegral (secs :: Int)) (localTimeToUTC ist (LocalTime day midnight))
