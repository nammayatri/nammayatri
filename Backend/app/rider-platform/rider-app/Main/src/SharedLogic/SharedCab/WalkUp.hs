-- | Walk-up: scanning or typing a cab's plate resolves it to its live session and route, which prefills the rider's search.
module SharedLogic.SharedCab.WalkUp
  ( liveSharedCab,
    liveSharedCabByCode,
    CodeResolution (..),
    resolveCode,
    cabRouteRequest,
    stopPairLegs,
    withoutCabRows,
    cabRouteServiceTier,
    declaresSharedCab,
    cabRouteStations,
    isStickerCode,
    findSharedCabConfig,
    enabledCabConfig,
    walkUpCab,
    routeServiceability,
    cabVehicle,
    trackStopRoutes,
    passingCab,
  )
where

import qualified API.Types.UI.FRFSTicketService as ApiFRFS
import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.Char as Char
import Data.Either (partitionEithers)
import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Person as DP
import qualified Environment
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JLU
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.SharedCab.Allocation.Select as Select
import SharedLogic.SharedCab.Allocation.Types (parseLtsTimestamp)
import qualified SharedLogic.SharedCab.Approach as Approach
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Demand as Demand
import SharedLogic.SharedCab.Enabled (requireSharedCabEnabled, sharedCabEnabled)
import SharedLogic.SharedCab.Interest (bestEffort, recordInterest)
import SharedLogic.SharedCab.LegState (isSharedCabAgency)
import SharedLogic.SharedCab.RateLimit (enforceStickerCodeLimit)
import qualified SharedLogic.SharedCab.RouteSnapshot as RouteSnapshot
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (..))
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Person as QP
import Tools.Error

liveSharedCab :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
liveSharedCab plate = mfilter ((== ACTIVE) . (.status)) <$> Session.readSession plate

isStickerCode :: Text -> Bool
isStickerCode code = T.length code == 4 && T.all Char.isDigit code

data CodeResolution = NoCab | OneCab Text | ManyCabs
  deriving (Show, Eq)

resolveCode :: Text -> [Text] -> CodeResolution
resolveCode code plates = case nub (filter ((== code) . T.takeEnd 4) plates) of
  [] -> NoCab
  [plate] -> OneCab plate
  _ -> ManyCabs

cabRouteRequest :: Maybe [Text] -> [Text] -> Bool
cabRouteRequest Nothing _ = False
cabRouteRequest (Just feedRouteCodes) requested = not (null requested) && all (`elem` feedRouteCodes) requested

stopEta :: UTCTime -> LT.UpcomingStop -> CQMMB.BusStopETA
stopEta now u = CQMMB.BusStopETA u.stop.stopCode (Just u.stop.name) u.eta (floor (utcTimeToPOSIXSeconds u.eta)) (Just (max 0 (floor (diffUTCTime u.eta now))))

-- | A stop-search request names no route: the cab feed's routes between the two stops (or, with only a source stop, the routes
-- passing it) make it a cab request. No such route leaves it on the bus path.
stopPairLegs :: Maybe Text -> [Text] -> Maybe [ApiTypes.RouteCodesWithLeg]
stopPairLegs (Just _) routes@(_ : _) = Just [ApiTypes.RouteCodesWithLeg {legOrder = 0, routeCodes = routes}]
stopPairLegs _ _ = Nothing

-- | The tier a route of this feed advertises in the public-transport data: a shared-cab route says SHARED_CAB, but only to a client
-- that declared the tier (`newServiceTiers`), so every other client gets exactly the data it got before.
cabRouteServiceTier :: Bool -> Maybe [Spec.ServiceTierType] -> Maybe Spec.ServiceTierType
cabRouteServiceTier isCabFeed declared = Spec.SHARED_CAB <$ guard (isCabFeed && declaresSharedCab declared)

-- | The client asked for the shared-cab tier: the app's shared-cab entry does, no bus client does.
declaresSharedCab :: Maybe [Spec.ServiceTierType] -> Bool
declaresSharedCab = maybe False (Spec.SHARED_CAB `elem`)

-- | The bus fleet's configs of a city: the shared-cab feed shares the BUS category but is never a bus.
withoutCabRows :: (a -> Text) -> [a] -> [a]
withoutCabRows agencyKeyOf = filter (not . isSharedCabAgency . agencyKeyOf)

-- | A shared-cab route has no scheduled trip (frequency feed), so stops and shape come from the route's stop mapping, in stop order.
cabRouteStations :: Id DIBC.IntegratedBPPConfig -> Text -> Maybe Text -> Text -> [(Text, Text, Int, LatLong)] -> [ApiFRFS.FRFSStationAPI]
cabRouteStations integratedBppConfigId routeCode color longName stops =
  [ ApiFRFS.FRFSStationAPI
      { address = Nothing,
        code,
        color,
        distance = Nothing,
        integratedBppConfigId,
        lat = Just point.lat,
        lon = Just point.lon,
        name = Just name,
        parentStopCode = Nothing,
        routeCodes = Just [routeCode],
        routeDetails = Just longName,
        sequenceNum = Just sequenceNum,
        stationType = Nothing,
        timeTakenToTravelUpcomingStop = Nothing,
        towards = Nothing
      }
    | (code, name, sequenceNum, point) <- sortOn (\(_, _, sequenceNum, _) -> sequenceNum) stops
  ]

liveSharedCabByCode :: DIBC.IntegratedBPPConfig -> Text -> Environment.Flow CodeResolution
liveSharedCabByCode integratedBppConfig code = do
  routes <- OTPRest.getRoutesByGtfsId integratedBppConfig
  plates <- concatMap (map (.session.vehicleNumber)) <$> mapM (RouteSnapshot.routeSnapshot . (.code)) routes
  pure (resolveCode code plates)

-- | A live cab as the bus builder's live vehicle: its route on its feed, tagged SHARED_CAB. A cab has no waybill or schedule.
cabLiveRoute :: Session -> Environment.Flow (DIBC.IntegratedBPPConfig, JLU.VehicleLiveRouteInfo)
cabLiveRoute s = do
  integratedBppConfig <- CQIBC.findById s.integratedBppConfigId >>= fromMaybeM IntegratedBPPConfigNotFound
  pure
    ( integratedBppConfig,
      JLU.VehicleLiveRouteInfo
        { routeCode = Just s.routeCode,
          serviceType = Spec.SHARED_CAB,
          vehicleNumber = s.vehicleNumber,
          routeNumber = Nothing,
          waybillId = Nothing,
          waybillNo = Nothing,
          scheduleNo = Nothing,
          tripNumber = Nothing,
          depot = Nothing,
          remaining_trip_details = Nothing,
          isActuallyValid = Nothing,
          busConductorId = Nothing,
          busDriverId = Nothing,
          busTagNumber = Nothing,
          busVehicleVariant = Nothing,
          eligiblePassIds = Nothing,
          serviceSubTypes = Nothing,
          seatLayoutId = Nothing,
          isHistoric = Nothing,
          scheduleBasedActiveTrip = Nothing,
          waybillStatus = Nothing
        }
    )

-- | The city's shared-cab feed sits on its own config, told apart by its SHARED_CAB agency.
findSharedCabConfig :: Maybe (Id DP.Person) -> Environment.Flow (Maybe DIBC.IntegratedBPPConfig)
findSharedCabConfig mbPersonId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  city <- QP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
  cabConfig <-
    IM.withInMemCache ["SharedCabConfig", city.merchantOperatingCityId.getId] 300 $
      find (isSharedCabAgency . (.agencyKey)) <$> SIBC.findAllIntegratedBPPConfig city.merchantOperatingCityId Enums.BUS DIBC.APPLICATION
  -- a suspended city lists no routes and resolves no codes
  enabledCabConfig (sharedCabEnabled city.merchantOperatingCityId) cabConfig

-- | The cab config while the city is on; a city without one (every bus-only city) never reads the switch.
enabledCabConfig :: Monad m => m Bool -> Maybe a -> m (Maybe a)
enabledCabConfig isEnabled = maybe (pure Nothing) (\cabConfig -> (cabConfig <$) . guard <$> isEnabled)

-- | The shared-cab entry's lookup: a live cab by its plate, else by its sticker code (the plate's last four) among the city's live
-- cabs. It never asks the bus system; a code no cab answers fails as an unknown bus number does, which the app reads as a wrong code.
walkUpCab :: Maybe (Id DP.Person) -> Text -> Environment.Flow (DIBC.IntegratedBPPConfig, JLU.VehicleLiveRouteInfo)
walkUpCab mbPersonId vehicleNumber =
  liveSharedCab vehicleNumber >>= \case
    Just session -> requireSharedCabEnabled session.merchantOperatingCityId >> cabLiveRoute session
    Nothing -> do
      resolution <-
        if isStickerCode vehicleNumber
          then
            findSharedCabConfig mbPersonId >>= \case
              Nothing -> pure NoCab
              Just ibc -> do
                whenJust mbPersonId (enforceStickerCodeLimit . getId)
                liveSharedCabByCode ibc vehicleNumber
          else pure NoCab
      case resolution of
        OneCab plate -> liveSharedCab plate >>= maybe notFound cabLiveRoute
        ManyCabs -> throwError CodeAmbiguous
        NoCab -> notFound
  where
    notFound = throwError (InvalidVehicleNumber $ "Vehicle " <> vehicleNumber <> ", not found on any route")

routeServiceability :: Maybe (Id DP.Person) -> ApiTypes.RouteServiceabilityReq -> Environment.Flow (Maybe ApiTypes.RouteServiceabilityResp)
routeServiceability mbPersonId req =
  findSharedCabRoutes >>= \case
    Just (cabIbc, legReqs, feedRouteCodes)
      | cabRouteRequest feedRouteCodes (concatMap (.routeCodes) legReqs) ->
        authenticateRider >>= \person -> Just <$> sharedCabRouteServiceability person cabIbc req legReqs
    _ -> pure Nothing
  where
    authenticateRider = do
      personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
      QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    -- Only a city with a shared-cab feed pays for the route list; any failure leaves the request on the unchanged bus path.
    findSharedCabRoutes = case req.routeCodes of
      Just legReqs@(_ : _)
        | not (null (concatMap (.routeCodes) legReqs)) ->
          withTryCatch "routeServiceability:sharedCabFeed" (findSharedCabConfig mbPersonId) >>= \case
            Right (Just cabIbc) -> do
              feed <-
                withTryCatch "routeServiceability:sharedCabFeedRoutes" $
                  IM.withInMemCache ["SharedCabFeedRoutes:" <> cabIbc.id.getId] 300 (map (.code) <$> OTPRest.getRoutesByGtfsId cabIbc)
              pure $ Just (cabIbc, legReqs, either (const Nothing) Just feed)
            _ -> pure Nothing
      _ -> stopPairRoutes
    -- The app's stop search sends the two stop codes and no route.
    stopPairRoutes = case (req.sourceStopCode, req.destinationStopCode) of
      (Just src, Just dest) ->
        withTryCatch "routeServiceability:sharedCabStopPair" (findSharedCabConfig mbPersonId >>= traverse (\cabIbc -> (,) cabIbc <$> JLU.getRouteCodesFromTo src dest cabIbc)) >>= \case
          Right (Just (cabIbc, routes)) -> pure $ (\legs -> (cabIbc, legs, Just routes)) <$> stopPairLegs req.sourceStopCode routes
          _ -> pure Nothing
      -- the stop picked on its own: the routes of the cab feed that pass it
      (Just src, Nothing) ->
        withTryCatch "routeServiceability:sharedCabStop" (findSharedCabConfig mbPersonId >>= traverse (\cabIbc -> (,) cabIbc . nub . map (.routeCode) <$> OTPRest.getRouteStopMappingByStopCode src cabIbc)) >>= \case
          Right (Just (cabIbc, routes)) -> pure $ (\legs -> (cabIbc, legs, Just routes)) <$> stopPairLegs req.sourceStopCode routes
          _ -> pure Nothing
      _ -> pure Nothing

-- | The cab routes through a stop and the cabs approaching it, in the bus API's shape (the Home "Share Taxis" card reads it like the bus
-- stop view). Nothing when the city has no cab feed or the stop is not on it, so the bus path answers as before.
trackStopRoutes :: Maybe (Id DP.Person) -> Text -> Maybe Text -> Environment.Flow (Maybe [ApiTypes.PassingRoutes])
trackStopRoutes mbPersonId stopCode mbRouteCodes =
  withTryCatch "trackStopRoutes:sharedCabFeed" (findSharedCabConfig mbPersonId) >>= \case
    Right (Just cabIbc) -> do
      mappings <- either (const []) identity <$> withTryCatch "trackStopRoutes:stopRoutes" (OTPRest.getRouteStopMappingByStopCode stopCode cabIbc)
      let wanted = maybe (const True) (\codes -> (`elem` filter (not . T.null) (map T.strip (T.splitOn "," codes)))) mbRouteCodes
      case nub (map (.routeCode) mappings) of
        [] -> pure Nothing
        codes -> do
          now <- getCurrentTime
          cfg <- Config.getTunables cabIbc.merchantOperatingCityId
          let stopPoint = (.stopPoint) <$> listToMaybe mappings
          -- the rider opening a stand's cab view counts as waiting on the routes through it, and a failure to note that never fails the view
          whenJust mbPersonId $ \personId ->
            void $ bestEffort (logError . ("shared-cab interest write failed: " <>)) (recordInterest cfg codes personId.getId >> Demand.recordSearch cabIbc.merchantOperatingCityId.getId personId.getId stopCode)
          Just <$> forM (filter wanted codes) (passingRoute cabIbc now cfg stopPoint)
    _ -> pure Nothing
  where
    passingRoute cabIbc now cfg stopPoint code = do
      mbRoute <- OTPRest.getRouteByRouteId cabIbc code
      snapshot <- RouteSnapshot.routeSnapshot code
      let cabs = flip mapMaybe snapshot $ \c -> do
            track <- c.track
            u <- Approach.cabStopEta now cfg stopCode (fromMaybe (LatLong track.vehicleInfo.latitude track.vehicleInfo.longitude) stopPoint) track
            pure (passingCab c.session.vehicleNumber (stopEta now u))
      stops <- OTPRest.getRouteStopMappingByRouteCode code cabIbc
      pure
        ApiTypes.PassingRoutes
          { routeCode = code,
            routeShortName = maybe code (.shortName) mbRoute,
            liveVehicles = sortOn (.eta.arrivalTimeUnix) cabs,
            schedules = [],
            isLastStop = fmap (.stopCode) (listToMaybe (sortOn (Down . (.sequenceNum)) stops)) == Just stopCode
          }

-- | A cab approaching a stop as any rider sees it: the plate shows its last four only, as in routeServiceability.
passingCab :: Text -> CQMMB.BusStopETA -> ApiTypes.PassingVehicleInfo
passingCab plate eta =
  ApiTypes.PassingVehicleInfo
    { vehicleNumber = T.takeEnd 4 plate,
      serviceTierType = Spec.SHARED_CAB,
      serviceTierName = Nothing,
      eta
    }

sharedCabRouteServiceability ::
  DP.Person ->
  DIBC.IntegratedBPPConfig ->
  ApiTypes.RouteServiceabilityReq ->
  [ApiTypes.RouteCodesWithLeg] ->
  Environment.Flow ApiTypes.RouteServiceabilityResp
sharedCabRouteServiceability person cabIbc req legReqs = do
  now <- getCurrentTime
  cfg <- Config.getTunables person.merchantOperatingCityId
  -- opening the cab list for a boarding stop is a search for a cab there
  whenJust req.sourceStopCode $ \stopCode ->
    void $ bestEffort (logError . ("shared-cab search not recorded: " <>)) (Demand.recordSearch cabIbc.merchantOperatingCityId.getId person.id.getId stopCode)
  legs <- forM legReqs $ \legReq -> do
    routes <- forM (nub legReq.routeCodes) $ \code -> do
      mbRoute <- OTPRest.getRouteByRouteId cabIbc code
      stops <- sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode code cabIbc
      snapshot <- RouteSnapshot.routeSnapshot code
      let wanted = filter (\c -> maybe True (\plate -> c.session.vehicleNumber == plate) req.vehicleNumber) snapshot
          boardStop = req.sourceStopCode >>= \stopCode -> (stopCode,) . (.stopPoint) <$> find ((== stopCode) . (.stopCode)) stops
          cabOf c =
            let load = Seats.riderLoad (Seats.cabSeatsOn (map (.stopCode) stops) c.session) (RouteSnapshot.cabBitmapsOf c) req.sourceStopCode req.destinationStopCode ((.vehicleInfo) <$> c.track)
             in cabVehicle now cfg (isJust req.vehicleNumber) boardStop load c.session c.track
          (scheduled, live) = partitionEithers (map cabOf wanted)
          boardEta v = (.arrivalTimeUnix) <$> (find (\e -> Just e.stopCode == req.sourceStopCode) =<< v.eta)
      pure
        ApiTypes.RouteWithLiveVehicle
          { routeCode = code,
            routeShortName = maybe code (.shortName) mbRoute,
            overrideSourceStopCode = Nothing,
            overrideDestinationStopCode = Nothing,
            liveVehicles = sortOn (\v -> (isNothing (boardEta v), boardEta v)) live,
            routeTag = Nothing,
            schedules = scheduled
          }
    pure ApiTypes.LegRouteWithLiveVehicle {legOrder = legReq.legOrder, routeWithLiveVehicles = routes}
  pure ApiTypes.RouteServiceabilityResp {legs, effectiveStops = Nothing, alternateRouteCodes = Nothing, pickEnabled = Just cfg.pickEnabled}

-- | One cab in the rider's route view, named by its trip (the pick's cabId) and the plate's last four characters; the whole plate only
-- when the request asks for that cab by it. A fresh fix lists it live, the board stop's ETA kept only when the allocator would believe
-- it. A cab without a fresh fix stays listed, and pickable, as a schedule.
cabVehicle ::
  UTCTime ->
  Config.SharedCabTunables ->
  Bool ->
  Maybe (Text, LatLong) ->
  (Int, Int) ->
  Session ->
  Maybe LT.VehicleTrackingOnRouteResp ->
  Either ApiTypes.ScheduledVehicleInfo ApiTypes.LiveVehicleInfo
cabVehicle now cfg namedByPlate mbBoardStop (freeSeats, ridersOnBoard) s = \case
  Just track
    | Select.isFreshPosition now cfg.ltsMaxAgeSec track.vehicleInfo ->
      Right
        ApiTypes.LiveVehicleInfo
          { number = shownPlate,
            vehicleTagNumber = Nothing,
            currentTripId = Just s.vehicleTripId.getId,
            serviceTierType = Spec.SHARED_CAB,
            vehicleVariant = Nothing,
            serviceTierName = Nothing,
            locationUTCTimestamp = fixTime track.vehicleInfo,
            eta = Just [stopEta now u | u <- fromMaybe [] track.vehicleInfo.upcomingStops, u.status == LT.Upcoming, believable track u],
            position = LatLong track.vehicleInfo.latitude track.vehicleInfo.longitude,
            serviceSubTypes = Nothing,
            seatSelectionType = Nothing,
            isUpcomingTrip = Nothing,
            previousRouteId = Nothing,
            bearing = Nothing,
            availableSeats = Just freeSeats,
            occupiedSeats = Just ridersOnBoard
          }
  mbTrack ->
    Left
      ApiTypes.ScheduledVehicleInfo
        { vehicleNumber = shownPlate,
          vehicleTagNumber = Nothing,
          vehicleVariant = Nothing,
          tripId = Just s.vehicleTripId.getId,
          isActiveTrip = Nothing,
          serviceTierType = Spec.SHARED_CAB,
          serviceTierName = Nothing,
          locationUTCTimestamp = fixTime . (.vehicleInfo) <$> mbTrack,
          eta = Nothing,
          position = (\track -> LatLong track.vehicleInfo.latitude track.vehicleInfo.longitude) <$> mbTrack,
          serviceSubTypes = Nothing,
          availableSeats = Just freeSeats,
          seatSelectionType = Nothing
        }
  where
    shownPlate = if namedByPlate then s.vehicleNumber else T.takeEnd 4 s.vehicleNumber
    fixTime vi = fromMaybe now (vi.timestamp >>= parseLtsTimestamp)
    believable track u = case mbBoardStop of
      Just (stopCode, stopPoint) | u.stop.stopCode == stopCode -> isJust (Approach.cabStopEta now cfg stopCode stopPoint track)
      _ -> True
