module Domain.Action.Dashboard.AppManagement.ShuttleInterest
  ( getShuttleInterestList,
    getShuttleInterestPublishedRoutes,
    getShuttleInterestMapPoints,
    postShuttleInterestShuttleRoutesPublish,
  )
where

import qualified API.Types.Dashboard.AppManagement.ShuttleInterest as ShuttleInterestAPI
import qualified BecknV2.FRFS.Enums as FRFSEnums
import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Codec.Archive.Zip as Zip
import Control.Applicative ((<|>))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (lefts, rights)
import Data.Hashable (hash)
import qualified Data.IntMap.Strict as IntMap
import Data.List (elemIndex, partition, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, todHour, todMin, utctDay)
import qualified Domain.Types.FRFSFarePolicy as DFRFSFarePolicy
import Domain.Types.FRFSQuoteCategoryType (FRFSQuoteCategoryType (ADULT))
import qualified Domain.Types.FRFSRouteFareProduct as DFRFSRouteFareProduct
import qualified Domain.Types.FRFSVehicleServiceTier as DFRFSVehicleServiceTier
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified "this" Domain.Types.PassOrganization as DPassOrganization
import qualified Domain.Types.PassType as DPassType
import qualified "this" Domain.Types.Person as DPerson
import qualified "this" Domain.Types.ShuttleInterest as DShuttleInterest
import qualified Domain.Types.ShuttlePublishedTrip as DShuttlePublishedTrip
import qualified Domain.Types.StopFare as DStopFare
import qualified Domain.Utils as DU
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Id as Id
import Kernel.Types.TimeBound (TimeBound (Unbounded))
import Kernel.Utils.Common (Currency (INR), DistanceUnit (Meter), HighPrecMoney, Meters (..), Seconds (..), fromMaybeM, generateGUID, getCurrentTime, logError, logInfo, throwError, withTryCatch)
import qualified SharedLogic.CorporateShuttle.RoutePlanner as RP
import qualified SharedLogic.CorporateShuttle.TransitHubs as TransitHubs
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import qualified Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import qualified Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import qualified Storage.Queries.MerchantOperatingCity as QMerchantOperatingCity
import qualified Storage.Queries.PassOrganization as QPassOrganization
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ShuttleInterest as QShuttleInterest
import qualified Storage.Queries.ShuttlePublishedTrip as QShuttlePublishedTrip
import qualified Storage.Queries.StopFare as QStopFare
import Tools.Error
import qualified Tools.Maps as Maps

-- | The map shows every matching interest at once; this caps one response.
maxMapPoints :: Int
maxMapPoints = 5000

-- | Interest forms of the dashboard's city, newest first, with the employee's and the company's name, for the
-- control-centre table. passOrganizationId, or companyDomain (one of the company's email domains, e.g. acme.example),
-- narrows it to one company.
getShuttleInterestList ::
  ( Id.ShortId DMerchant.Merchant ->
    Context.City ->
    Maybe (Id.Id DPassOrganization.PassOrganization) ->
    Maybe Text ->
    Maybe Int ->
    Maybe Int ->
    Environment.Flow ShuttleInterestAPI.ShuttleInterestListResp
  )
getShuttleInterestList merchantShortId opCity mbPassOrganizationId mbCompanyDomain mbLimit mbOffset = do
  merchantOperatingCity <- findCity merchantShortId opCity
  let limit = min 100 (max 1 (fromMaybe 20 mbLimit))
      offset = max 0 (fromMaybe 0 mbOffset)
  companies <- QPassOrganization.findByMerchantOperatingCityIdAndPassEnum merchantOperatingCity.id DPassType.RegularPass
  interests <- findInterests merchantOperatingCity.id companies mbPassOrganizationId mbCompanyDomain limit offset
  employeeNames <- getEmployeeNames interests
  let companyNames = Map.fromList [(company.id, company.name) | company <- companies]
  pure $
    ShuttleInterestAPI.ShuttleInterestListResp
      { ShuttleInterestAPI.interests = map (mkItem employeeNames companyNames) interests,
        ShuttleInterestAPI.limit = limit,
        ShuttleInterestAPI.offset = offset
      }

-- | Pickup and drop coordinates of the city's interests (optionally one company's), for the control-centre map.
getShuttleInterestMapPoints ::
  ( Id.ShortId DMerchant.Merchant ->
    Context.City ->
    Maybe (Id.Id DPassOrganization.PassOrganization) ->
    Maybe Text ->
    Environment.Flow ShuttleInterestAPI.ShuttleInterestMapResp
  )
getShuttleInterestMapPoints merchantShortId opCity mbPassOrganizationId mbCompanyDomain = do
  merchantOperatingCity <- findCity merchantShortId opCity
  companies <- QPassOrganization.findByMerchantOperatingCityIdAndPassEnum merchantOperatingCity.id DPassType.RegularPass
  interests <- findInterests merchantOperatingCity.id companies mbPassOrganizationId mbCompanyDomain maxMapPoints 0
  employeeNames <- getEmployeeNames interests
  let companyNames = Map.fromList [(company.id, company.name) | company <- companies]
  pure $ ShuttleInterestAPI.ShuttleInterestMapResp {ShuttleInterestAPI.points = map (mkMapPoint employeeNames companyNames) interests}

findCity :: Id.ShortId DMerchant.Merchant -> Context.City -> Environment.Flow DMOC.MerchantOperatingCity
findCity merchantShortId opCity =
  QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")

-- | The city's interests, or one company's: by passOrganizationId, else by companyDomain resolved to the company that owns
-- it (a domain no company owns has no interests).
findInterests ::
  Id.Id DMOC.MerchantOperatingCity ->
  [DPassOrganization.PassOrganization] ->
  Maybe (Id.Id DPassOrganization.PassOrganization) ->
  Maybe Text ->
  Int ->
  Int ->
  Environment.Flow [DShuttleInterest.ShuttleInterest]
findInterests merchantOperatingCityId companies mbPassOrganizationId mbCompanyDomain limit offset =
  case (mbPassOrganizationId, mbDomain) of
    (Just passOrganizationId, _) -> byCompany passOrganizationId
    (Nothing, Just domain) -> maybe (pure []) (byCompany . (.id)) $ find (\company -> domain `elem` company.emailDomains) companies
    (Nothing, Nothing) -> QShuttleInterest.findAllByMerchantOperatingCityId (Just limit) (Just offset) merchantOperatingCityId
  where
    mbDomain = mfilter (not . T.null) (T.toLower . T.strip <$> mbCompanyDomain)
    byCompany = QShuttleInterest.findAllByMerchantOperatingCityIdAndPassOrganizationId (Just limit) (Just offset) merchantOperatingCityId

getEmployeeNames :: [DShuttleInterest.ShuttleInterest] -> Environment.Flow (Map.Map (Id.Id DPerson.Person) Text)
getEmployeeNames [] = pure Map.empty
getEmployeeNames interests = do
  persons <- QPerson.findAllByIds (map (.personId) interests)
  pure $ Map.fromList [(person.id, name) | person <- persons, Just name <- [fullName person]]
  where
    fullName person =
      let name = T.unwords (catMaybes [person.firstName, person.lastName])
       in if T.null name then Nothing else Just name

mkItem ::
  Map.Map (Id.Id DPerson.Person) Text ->
  Map.Map (Id.Id DPassOrganization.PassOrganization) Text ->
  DShuttleInterest.ShuttleInterest ->
  ShuttleInterestAPI.ShuttleInterestItem
mkItem employeeNames companyNames interest =
  ShuttleInterestAPI.ShuttleInterestItem
    { ShuttleInterestAPI.personId = interest.personId,
      ShuttleInterestAPI.employeeName = Map.lookup interest.personId employeeNames,
      ShuttleInterestAPI.passOrganizationId = interest.passOrganizationId,
      ShuttleInterestAPI.companyName = Map.lookup interest.passOrganizationId companyNames,
      ShuttleInterestAPI.companyDomain = interest.companyDomain,
      ShuttleInterestAPI.pickupLat = interest.pickupLat,
      ShuttleInterestAPI.pickupLon = interest.pickupLon,
      ShuttleInterestAPI.pickupAddress = interest.pickupAddress,
      ShuttleInterestAPI.dropLat = interest.dropLat,
      ShuttleInterestAPI.dropLon = interest.dropLon,
      ShuttleInterestAPI.dropAddress = interest.dropAddress,
      ShuttleInterestAPI.tripType = interest.tripType,
      ShuttleInterestAPI.pickupTime = interest.pickupTime,
      ShuttleInterestAPI.dropTime = interest.dropTime,
      ShuttleInterestAPI.daysOfWeek = interest.daysOfWeek,
      ShuttleInterestAPI.createdAt = interest.createdAt,
      ShuttleInterestAPI.updatedAt = interest.updatedAt
    }

-- | The employee's saved pickup and drop, exactly as stored (never moved or generated), with their name and company.
mkMapPoint ::
  Map.Map (Id.Id DPerson.Person) Text ->
  Map.Map (Id.Id DPassOrganization.PassOrganization) Text ->
  DShuttleInterest.ShuttleInterest ->
  ShuttleInterestAPI.ShuttleInterestMapPoint
mkMapPoint employeeNames companyNames interest =
  ShuttleInterestAPI.ShuttleInterestMapPoint
    { ShuttleInterestAPI.personId = interest.personId,
      ShuttleInterestAPI.employeeName = Map.lookup interest.personId employeeNames,
      ShuttleInterestAPI.passOrganizationId = interest.passOrganizationId,
      ShuttleInterestAPI.companyName = Map.lookup interest.passOrganizationId companyNames,
      ShuttleInterestAPI.companyDomain = interest.companyDomain,
      ShuttleInterestAPI.tripType = interest.tripType,
      ShuttleInterestAPI.pickupLat = interest.pickupLat,
      ShuttleInterestAPI.pickupLon = interest.pickupLon,
      ShuttleInterestAPI.pickupAddress = interest.pickupAddress,
      ShuttleInterestAPI.dropLat = interest.dropLat,
      ShuttleInterestAPI.dropLon = interest.dropLon,
      ShuttleInterestAPI.dropAddress = interest.dropAddress
    }

-- | The city's corporate shuttle routes, planned from the interest forms (the same plan the GTFS download gives): each
-- with its stops in order and its trips with the days they run. With a day (e.g. monday), only the routes that run that
-- day, with that day's trips and riders.
getShuttleInterestPublishedRoutes :: Id.ShortId DMerchant.Merchant -> Context.City -> Maybe Text -> Environment.Flow ShuttleInterestAPI.ShuttlePublishedRoutesResp
getShuttleInterestPublishedRoutes merchantShortId opCity mbDay = do
  merchantOperatingCity <- findCity merchantShortId opCity
  findCorpConfig merchantOperatingCity >>= \case
    Nothing -> pure ShuttleInterestAPI.ShuttlePublishedRoutesResp {ShuttleInterestAPI.feedKey = Nothing, ShuttleInterestAPI.routes = []}
    Just config -> do
      planned <- planCity config merchantOperatingCity
      let day = mfilter (not . T.null) (T.toLower . T.strip <$> mbDay)
          shown = [(r, ts) | (r, all') <- planned, let ts = filter (\t -> maybe True (== t.day) day) all', not (null ts)]
      pure
        ShuttleInterestAPI.ShuttlePublishedRoutesResp
          { ShuttleInterestAPI.feedKey = Just config.feedKey,
            ShuttleInterestAPI.routes = [mkPublishedRoute r ts | (r, ts) <- shown]
          }

mkPublishedRoute :: PlannedRoute -> [DShuttlePublishedTrip.ShuttlePublishedTrip] -> ShuttleInterestAPI.ShuttlePublishedRoute
mkPublishedRoute route timetable =
  ShuttleInterestAPI.ShuttlePublishedRoute
    { ShuttleInterestAPI.routeCode = route.routeCode,
      ShuttleInterestAPI.routeType = route.routeKind,
      ShuttleInterestAPI.distanceMeters = Just route.distanceMeters,
      ShuttleInterestAPI.durationMinutes = (.minutesFromStart) <$> lastMay route.stops,
      ShuttleInterestAPI.shortName = T.replace "-" " " (T.drop 5 route.routeCode),
      ShuttleInterestAPI.name = route.name,
      ShuttleInterestAPI.tripCount = Just (length trips),
      ShuttleInterestAPI.firstDeparture = (.departure) <$> listToMaybe trips,
      ShuttleInterestAPI.stops = zipWith mkStop [1 ..] route.stops,
      ShuttleInterestAPI.trips = trips,
      ShuttleInterestAPI.passOrganizationId = route.passOrganizationId,
      ShuttleInterestAPI.fareAmount = route.fareAmount,
      ShuttleInterestAPI.shape = route.shape
    }
  where
    trips = mkTrips route.tripSlots timetable
    mkStop seqNum stop =
      ShuttleInterestAPI.ShuttlePublishedStop
        { ShuttleInterestAPI.stopCode = stop.stopCode,
          ShuttleInterestAPI.name = stop.name,
          ShuttleInterestAPI.lat = stop.lat,
          ShuttleInterestAPI.lon = stop.lon,
          ShuttleInterestAPI.sequence = seqNum,
          ShuttleInterestAPI.minutesFromStart = Just stop.minutesFromStart
        }

-- | The route's trips from its timetable rows (one row per trip and day), in departure order, days in week order.
mkTrips :: Map.Map Text (Text, Text) -> [DShuttlePublishedTrip.ShuttlePublishedTrip] -> [ShuttleInterestAPI.ShuttlePublishedTripInfo]
mkTrips slots rows =
  [ ShuttleInterestAPI.ShuttlePublishedTripInfo
      { ShuttleInterestAPI.tripId = tripId,
        ShuttleInterestAPI.departure = maybe "" (.departure) (listToMaybe tripRows),
        ShuttleInterestAPI.days = sortOn dayIndex (map (.day) tripRows),
        ShuttleInterestAPI.riders = maximum (0 : map (.riders) tripRows),
        ShuttleInterestAPI.slot = fst <$> Map.lookup tripId slots,
        ShuttleInterestAPI.direction = snd <$> Map.lookup tripId slots
      }
    | (tripId, tripRows) <- sortOn (fmap (.departure) . listToMaybe . snd) (Map.toList (Map.fromListWith (<>) [(r.tripId, [r]) | r <- rows]))
  ]

-- | The city's corporate shuttle feed (corp_<city>): its integrated BPP config is set up once per city, as the shared cab's.
findCorpConfig :: DMOC.MerchantOperatingCity -> Environment.Flow (Maybe DIBC.IntegratedBPPConfig)
findCorpConfig merchantOperatingCity =
  find (\config -> "corp_" `T.isPrefixOf` config.feedKey) <$> SIBC.findAllIntegratedBPPConfig merchantOperatingCity.id BecknSpec.BUS DIBC.MULTIMODAL

requireCorpConfig :: DMOC.MerchantOperatingCity -> Environment.Flow DIBC.IntegratedBPPConfig
requireCorpConfig merchantOperatingCity =
  findCorpConfig merchantOperatingCity >>= fromMaybeM (InvalidRequest "The corporate shuttle feed (corp_<city>) is not configured for this city")

-- Planning ------------------------------------------------------------------------------------------------------------
-- From one company's interest forms (SharedLogic.CorporateShuttle.RoutePlanner has the algorithms):
--   pickup points  shared by both directions and every day: a metro station, a bus stand or a main road (told by the road
--                  network's own speed on it, 28 km/h and more, not by names), never a side street or a doorstep.
--                  Neighbours share the point they can all walk to, up to a kilometre along the streets (stations and
--                  stands first); someone with none within a kilometre, or alone 3 km from everyone else, is left off
--   routes         one set for every slot and day, so a route keeps its id whenever it runs: Clarke-Wright savings, moves
--                  between routes and VND stop order on the road travel times both ways, joining points while it saves
--                  driving and no slot has more riders than a shuttle's 6 seats; a group runs as a loop (office, stops,
--                  office) or straight (to the office, and from it), whichever its geography suits
--   shuttles       two slots each way for the whole city, the half-hour pair that best fits everyone's times on the forms;
--                  everyone rides the slot nearer their time; a route runs one shuttle in each slot it has riders in (more
--                  at the same time only when one stop alone has more riders than seats), reaching the office at the
--                  morning slot and leaving it at the evening slot
--   roads          the shuttle never drives into a road and back out the same way: such a stop moves to where that road
--                  leaves the route, and stops that end up together are one; a stop is named after its station or road

snapMaxMeters, nameMaxMeters :: Double
snapMaxMeters = 200
nameMaxMeters = 500

matrixChunk, pairChunk :: Int
matrixChunk = 10
-- origin-destination pairs per OSRM one-to-one request
pairChunk = 40

gridMeters, classProbeMeters, mainRoadKmh, roadKmh :: Double
-- road points are sampled at grid points this far apart over everyone's walking area
gridMeters = 200
-- a road's speed is read over this stretch of it, along it either way
classProbeMeters = 100
-- the road network's speed on a main road (tertiary and up), and on an ordinary road (residential); slower is a lane
mainRoadKmh = 28
roadKmh = 17

weekDays :: [Text]
weekDays = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]

dayIndex :: Text -> Int
dayIndex d = fromMaybe 7 (elemIndex d weekDays)

-- | An employee: where they live (their saved pickup, exactly), when they want to reach the office and leave it (minutes
-- after midnight), and on which days.
data Rider = Rider {riderId :: Text, home :: Maps.LatLong, toOffice :: Maybe Int, fromOffice :: Maybe Int, riderDays :: [Text]}

-- | A shared pickup point (also where the evening shuttle drops them): a metro station, a bus stand or a main road, with
-- its name and who uses it.
data Pickup = Pickup {pickupAt :: Maps.LatLong, pickupRoad :: Maybe Text, pickupRiders :: [Text]}

-- | A planned route: one company's, in the city's corp feed.
-- tripSlots: per trip id, the slot time its employees asked for (HH:MM) and its direction (TO_OFFICE / FROM_OFFICE).
data PlannedRoute = PlannedRoute {routeCode :: Text, routeKind :: Text, passOrganizationId :: Id.Id DPassOrganization.PassOrganization, name :: Text, fareAmount :: HighPrecMoney, stops :: [PlannedStop], shape :: [[Double]], distanceMeters :: Int, tripSlots :: Map.Map Text (Text, Text)}
  deriving (Generic, ToJSON, FromJSON)

data PlannedStop = PlannedStop {stopCode :: Text, name :: Text, lat :: Double, lon :: Double, minutesFromStart :: Int}
  deriving (Generic, ToJSON, FromJSON)

-- | Every company's routes in the city, each with its trips (one row per trip and day), planned from the interest forms.
-- A company without an office location has no routes yet.
planCity :: DIBC.IntegratedBPPConfig -> DMOC.MerchantOperatingCity -> Environment.Flow [(PlannedRoute, [DShuttlePublishedTrip.ShuttlePublishedTrip])]
planCity config merchantOperatingCity = do
  companies <- QPassOrganization.findByMerchantOperatingCityIdAndPassEnum merchantOperatingCity.id DPassType.RegularPass
  let corporate = filter (\c -> not (null c.emailDomains)) companies
  forM_ corporate $ \c ->
    when (any (\o -> o.id /= c.id && orgCode o.name == orgCode c.name) corporate) $
      throwError $
        InvalidRequest ("Two companies in this city get the same route code " <> orgCode c.name <> "; rename one of them")
  interestsByCompany <- forM corporate $ \c -> (c,) <$> allInterests merchantOperatingCity c
  -- the plan only changes when a form (or a company's office) does: it is kept until then, so showing the routes again
  -- is instant
  let fingerprint = hash (show [(c.id.getId, c.name, c.officeLat, c.officeLon, [(i.personId.getId, show i.updatedAt :: Text) | i <- is]) | (c, is) <- interestsByCompany] :: Text)
      cacheKey = "CorporateShuttle:Plan:" <> plannerVersion <> ":" <> merchantOperatingCity.id.getId <> ":" <> show fingerprint
  Redis.safeGet cacheKey >>= \case
    Just cached -> pure cached
    Nothing -> do
      let ridersByCompany = [(c, filter (\r -> isJust r.toOffice || isJust r.fromOffice) (ridersOf is)) | (c, is) <- interestsByCompany]
          -- the city's two slots each way, from everyone's times: the same two filters for every company and day
          everyone = concatMap snd ridersByCompany
          slots = DaySlots {morningSlots = twoSlots (mapMaybe (.toOffice) everyone), eveningSlots = twoSlots (mapMaybe (.fromOffice) everyone)}
      planned <- fmap concat . forM ridersByCompany $ \(c, rs) ->
        withTryCatch "shuttleRoutes:planCompany" (planCompany config merchantOperatingCity slots c rs) >>= \case
          Right routes -> pure routes
          Left err -> do
            logError ("Corporate shuttle: could not plan " <> c.name <> "'s routes: " <> show err)
            pure []
      Redis.setExp cacheKey planned (24 * 3600)
      logInfo ("Corporate shuttle: planned " <> show (length planned) <> " routes")
      pure planned

-- | Bumped whenever the planner changes, so a plan cached by an older planner is not used.
plannerVersion :: Text
plannerVersion = "v4"

-- | f over xs, up to parallelism at a time (independent maps-service calls). f must not fail: its result is dropped.
inParallel :: (a -> Environment.Flow b) -> [a] -> Environment.Flow [b]
inParallel f xs = concat <$> mapM (DU.mapConcurrently f) (chunksOf parallelism xs)

parallelism :: Int
parallelism = 16

-- | The slot times each way (minutes after midnight).
data DaySlots = DaySlots {morningSlots :: [Int], eveningSlots :: [Int]}

-- | The two slot times that best fit the times employees asked for: on the half hour, the pair that leaves everyone
-- the least total time from the slot nearer to them, the two at least an hour apart (one slot when everyone's times
-- are within an hour).
twoSlots :: [Int] -> [Int]
twoSlots [] = []
twoSlots wishes =
  let halfHour w = 30 * round (fromIntegral w / 30 :: Double)
      candidates = nubOrd (map halfHour wishes)
      misfit a b = sum [min (abs (w - a)) (abs (w - b)) | w <- wishes]
   in case sortOn fst [((misfit a b, a, b), [a, b]) | a <- candidates, b <- candidates, b - a >= 60] of
        (_, best) : _ -> best
        [] -> take 1 (map snd (sortOn fst [((misfit a a, a), a) | a <- candidates]))

planCompany :: DIBC.IntegratedBPPConfig -> DMOC.MerchantOperatingCity -> DaySlots -> DPassOrganization.PassOrganization -> [Rider] -> Environment.Flow [(PlannedRoute, [DShuttlePublishedTrip.ShuttlePublishedTrip])]
planCompany config merchantOperatingCity slots company riders = case (company.officeLat, company.officeLon) of
  (Just lat, Just lon) | not (null riders) -> planRiders (Maps.LatLong lat lon)
  _ -> pure []
  where
    org = orgCode company.name
    defaultFare = 40
    planRiders office = do
      officeAt <- maybe office (.location) <$> nearestRoad merchantOperatingCity office
      (pickups0, noStop) <- pickupPoints merchantOperatingCity riders
      -- someone alone far from everyone else (farMeters from every other pickup) is left off: a shuttle for one rider
      -- across town would cost every other route its seats
      let lonely p = length p.pickupRiders == 1 && all (\q -> q.pickupAt == p.pickupAt || RP.metersBetween p.pickupAt q.pickupAt > farMeters) pickups0
          (isolated, pickups) = partition lonely pickups0
          leftOff = noStop <> concatMap (.pickupRiders) isolated
      unless (null leftOff) $ logInfo ("Corporate shuttle: " <> company.name <> ": " <> show (length leftOff) <> " too far from a main road, metro or bus stand, or from everyone else, left off")
      let nodes = IntMap.fromList (zip [0 ..] (officeAt : map (.pickupAt) pickups))
          nodeOf = Map.fromList [(rid, k) | (k, p) <- zip [1 :: Int ..] pickups, rid <- p.pickupRiders]
          served = filter (\r -> Map.member r.riderId nodeOf) riders
      matrix <- roadMatrix merchantOperatingCity (IntMap.elems nodes)
      names <- IntMap.fromList <$> inParallel (\(k, p) -> (k,) <$> maybe (stopName merchantOperatingCity p.pickupAt) pure p.pickupRoad) (zip [1 ..] pickups)
      let -- the slots each way; slot k to the office pairs with slot k home
          slotsOf morning = if morning then slots.morningSlots else slots.eveningSlots
          slotIndex morning w = maybe 0 snd (listToMaybe (sortOn fst [((abs (w - t), t), i) | (i, t) <- zip [0 :: Int ..] (slotsOf morning)]))
          dirOf morning = if morning then (.toOffice) else (.fromOffice)
          ridesIn day wave morning r = day `elem` r.riderDays && (slotIndex morning <$> dirOf morning r) == Just wave
          waves = [0 .. max 1 (max (length slots.morningSlots) (length slots.eveningSlots)) - 1]
          dayWaves = [(day, wave) | day <- weekDays, wave <- waves]
          loadsOf morning = filter (not . IntMap.null) [IntMap.fromListWith (+) [(nodeOf Map.! r.riderId, 1) | r <- served, ridesIn day wave morning r] | (day, wave) <- dayWaves]
          -- one set of routes for every slot and day: a route is its path, and its id is the same at any time it runs
          routes = RP.planRoutes matrix (loadsOf True) (loadsOf False) (IntMap.keys (IntMap.delete 0 nodes))
      driven <- forM routes $ \(kind, ps) -> do
        let ns = RP.nodesOf kind ps
        (points, stopIx, offsets, shape, meters) <- drive merchantOperatingCity [n /= 0 | n <- ns] (map (nodes IntMap.!) ns)
        let firstNode = IntMap.fromListWith (\_ a -> a) (zip stopIx ns)
        stopNames <- forM (zip [0 :: Int ..] points) $ \(i, (point, moved)) -> case IntMap.lookup i firstNode of
          Just 0 -> pure (company.name <> " office")
          Just node | not moved -> pure (IntMap.findWithDefault "Pickup point" node names)
          _ -> stopName merchantOperatingCity point
        let offsetOf = Map.fromList [(n, IntMap.findWithDefault 0 ix (IntMap.fromList (zip [0 ..] offsets))) | (n, ix) <- zip ns stopIx, n /= 0]
            atOffice = fromMaybe 0 (lastMay offsets)
            directions = case kind of
              RP.ToOffice -> [True]
              RP.FromOffice -> [False]
              RP.Loop -> [True, False]
            -- MORNING: the shuttle reaches the office at the slot time; EVENING: it leaves the office then. A slot
            -- busier than the seats runs more shuttles of the route at the same time (<route>-<HHMM>-2, ...)
            trips =
              [ (day, tid, dep, (t, if morning then "TO_OFFICE" else "FROM_OFFICE" :: Text), length grp)
                | (day, wave) <- dayWaves,
                  morning <- directions,
                  t <- take 1 (drop wave (slotsOf morning)),
                  let ids = [r.riderId | r <- served, ridesIn day wave morning r, Map.member (nodeOf Map.! r.riderId) offsetOf],
                  not (null ids),
                  -- a service day starts at midnight: a shuttle would leave before it leaves at 00:00 instead
                  let dep = max 0 (if morning then t - atOffice else t),
                  (k, grp) <- zip [1 :: Int ..] (chunksOf RP.seats ids),
                  let tid = tripIdOf "" dep <> (if k > 1 then "-" <> show k else "")
              ]
        pure (kind, [(point, label) | ((point, _), label) <- zip points stopNames], offsets, shape, meters, trips)
      let planned = sortOn (\(kind, _, _, _, _, trips) -> (kind, minimum (1440 : [d | (_, _, d, _, _) <- trips]))) (filter (\(_, _, _, _, _, trips) -> not (null trips)) driven)
          -- a stop is its place: two routes at one place share the stop
          spotKey (point, _) = (round (point.lat * 1e5) :: Integer, round (point.lon * 1e5) :: Integer)
          stopCodes = Map.fromList (zip (nubOrd [spotKey st | (_, sts, _, _, _, _) <- planned, st <- sts]) [1 :: Int ..])
      now <- getCurrentTime
      pure
        [ ( PlannedRoute {routeCode, routeKind = RP.routeKindText kind, passOrganizationId = company.id, name = routeName named, fareAmount = defaultFare, stops = named, shape, distanceMeters = meters, tripSlots = Map.fromList [(routeCode <> tid, (minutesToHHMM t, d)) | (_, tid, _, (t, d), _) <- trips]},
            [mkTripRow config merchantOperatingCity now routeCode (routeCode <> tid) day dep count | (day, tid, dep, _, count) <- trips]
          )
          | (n, (kind, sts, offsets, shape, meters, trips)) <- zip [1 :: Int ..] planned,
            let routeCode = "CORP-" <> org <> "-" <> show n
                named =
                  [ PlannedStop {stopCode = "CORP_" <> org <> "_S" <> show (stopCodes Map.! spotKey st), name = stopLabel, lat = point.lat, lon = point.lon, minutesFromStart = offset}
                    | (st@(point, stopLabel), offset) <- zip sts offsets
                  ]
        ]

-- | Someone this far (straight line) from every other employee's pickup, riding alone, is too far to serve.
farMeters :: Double
farMeters = 3000

-- | Every current interest form of the company, read page by page (none is left out of the plan).
allInterests :: DMOC.MerchantOperatingCity -> DPassOrganization.PassOrganization -> Environment.Flow [DShuttleInterest.ShuttleInterest]
allInterests merchantOperatingCity company = go 0
  where
    pageSize = 1000
    go offset = do
      page <- QShuttleInterest.findAllByMerchantOperatingCityIdAndPassOrganizationId (Just pageSize) (Just offset) merchantOperatingCity.id company.id
      if length page < pageSize then pure page else (page <>) <$> go (offset + pageSize)

-- | The employees, from their forms: their saved pickup exactly as they chose it, their times on the legs they asked for.
ridersOf :: [DShuttleInterest.ShuttleInterest] -> [Rider]
ridersOf interests =
  sortOn
    (.riderId)
    [ Rider
        { riderId = interest.personId.getId,
          home = Maps.LatLong interest.pickupLat interest.pickupLon,
          toOffice = if interest.tripType /= DShuttleInterest.TO_HOME then minutesOf <$> interest.pickupTime else Nothing,
          fromOffice = if interest.tripType /= DShuttleInterest.TO_OFFICE then minutesOf <$> interest.dropTime else Nothing,
          riderDays = days
        }
      | interest <- interests,
        let days = nubOrd (map (T.toLower . show) interest.daysOfWeek),
        not (null days)
    ]
  where
    minutesOf t = todHour t * 60 + todMin t

-- | The shared pickup points: per DBSCAN neighbourhood, chosen from the metro stations, bus stands and main-road points
-- across its walking area (a kilometre), everyone's walk then checked along the streets; whoever no shared point suits
-- walks to the nearest main road, metro or bus stand within a kilometre. Never a side street or someone's home. Also who
-- has none within walking distance (left off).
pickupPoints :: DMOC.MerchantOperatingCity -> [Rider] -> Environment.Flow ([Pickup], [Text])
pickupPoints merchantOperatingCity riders = do
  let -- riders at one spot (to about ten metres: one building) are one home
      spots = Map.elems (Map.fromListWith (flip (<>)) [(spot r.home, [r]) | r <- riders])
      homes = IntMap.fromList (zip [0 ..] [(RP.Home r0.home (length rs), map (.riderId) rs) | rs@(r0 : _) <- spots])
  allSites <- stopSites merchantOperatingCity (map (.homeAt) (map fst (IntMap.elems homes)))
  results <- forM (RP.dbscan (map fst (IntMap.elems homes))) $ \members -> do
    let byLocal = IntMap.fromList (zip [0 ..] (map (homes IntMap.!) members))
        hs = map fst (IntMap.elems byLocal)
        ridersAt i = maybe [] snd (IntMap.lookup i byLocal)
        homeAt i = maybe (Maps.LatLong 0 0) ((.homeAt) . fst) (IntMap.lookup i byLocal)
        sites = [st | st <- allSites, any (\h -> RP.walkEstimate h.homeAt st.siteAt <= RP.walkMeters) hs]
        (groups, unreachable) = RP.chooseSites hs sites
    checked <- inParallel `flip` groups $ \(site, is) -> do
      walks <- walkingMeters merchantOperatingCity (map homeAt is) site.siteAt
      pure (site, partition (\i -> maybe True (<= RP.walkMeters) (Map.lookup (spot (homeAt i)) walks)) is)
    own <- forM (unreachable <> concatMap (snd . snd) checked) $ \i ->
      ownStop merchantOperatingCity sites (homeAt i) <&> \case
        Just road -> Right Pickup {pickupAt = road.siteAt, pickupRoad = road.siteRoad, pickupRiders = ridersAt i}
        Nothing -> Left (ridersAt i)
    pure ([Pickup {pickupAt = site.siteAt, pickupRoad = site.siteRoad, pickupRiders = concatMap ridersAt ok} | (site, (ok, _)) <- checked, not (null ok)] <> rights own, concat (lefts own))
  -- two neighbourhoods' riders at one stop are one pickup
  let merged = Map.elems (Map.fromListWith (\b a -> a {pickupRiders = a.pickupRiders <> b.pickupRiders}) [(spot p.pickupAt, p) | p <- concatMap fst results])
  pure (merged, concatMap snd results)
  where
    spot p = (round (p.lat * 1e4) :: Integer, round (p.lon * 1e4) :: Integer)

-- | Where a shuttle may stop near the homes: the metro stations and bus stands within walking distance of anyone (at the
-- road beside them), then the main-road points (never a side street).
stopSites :: DMOC.MerchantOperatingCity -> [Maps.LatLong] -> Environment.Flow [RP.Site]
stopSites merchantOperatingCity homesAt = do
  let near h = any (\p -> RP.walkEstimate p (hubAt h) <= RP.walkMeters) homesAt
      hubAt h = Maps.LatLong h.hubLat h.hubLon
  hubs <- inParallel `flip` filter near TransitHubs.transitHubs $ \h -> do
    road <- nearestRoad merchantOperatingCity (hubAt h)
    pure (RP.Site (maybe (hubAt h) (.location) road) (Just h.hubName) RP.hubClass)
  roads <- filter (\st -> st.siteClass >= RP.mainRoadClass) <$> roadSites merchantOperatingCity homesAt
  pure (hubs <> roads)

-- | Every road point anyone could walk to: OSRM's nearest road segments around grid points gridMeters apart over each
-- home's walking radius (nearest alone only sees the segments right by a point, so a main road a few hundred metres
-- away would never be considered), one per ~40 m, each classed by the road network (classifyRoads).
roadSites :: DMOC.MerchantOperatingCity -> [Maps.LatLong] -> Environment.Flow [RP.Site]
roadSites merchantOperatingCity homesAt = do
  let lat0 = maybe 13 (.lat) (listToMaybe homesAt)
      dLat = gridMeters / 111000
      dLon = dLat / max 0.2 (cos (lat0 * pi / 180))
      reach = RP.walkMeters / 1.3 + gridMeters
      cellOf p = (floor (p.lat / dLat) :: Int, floor (p.lon / dLon) :: Int)
      span' = ceiling (reach / gridMeters) :: Int
      cells = nubOrd [(y + dy, x + dx) | h <- homesAt, let (y, x) = cellOf h, dy <- [- span' .. span'], dx <- [- span' .. span']]
      centreOf (y, x) = Maps.LatLong ((fromIntegral y + 0.5) * dLat) ((fromIntegral x + 0.5) * dLon)
      grid = [c | c <- map centreOf cells, any (\h -> RP.metersBetween h c <= reach) homesAt]
  found <- concat <$> inParallel (roadsNear merchantOperatingCity) (homesAt <> grid)
  let key (p, _) = (round (p.lat * 2.5e3) :: Integer, round (p.lon * 2.5e3) :: Integer)
      points = Map.elems (Map.fromListWith (\a b -> if isJust (snd b) && isNothing (snd a) then b else a) [(key r, r) | r <- found])
  classifyRoads merchantOperatingCity points

-- | Road segments near a point (the city's OSRM): where on each road, and its name (for naming the stop only).
roadsNear :: DMOC.MerchantOperatingCity -> Maps.LatLong -> Environment.Flow [(Maps.LatLong, Maybe Text)]
roadsNear merchantOperatingCity p = either (const []) identity <$> withTryCatch "shuttleRoutes:roadsNear" (roadsWithin RP.walkMeters merchantOperatingCity p)

-- | Road segments within metres of a point, cached (a found road only).
roadsWithin :: Double -> DMOC.MerchantOperatingCity -> Maps.LatLong -> Environment.Flow [(Maps.LatLong, Maybe Text)]
roadsWithin metres merchantOperatingCity p = do
  let cacheKey = "CorporateShuttle:Nearest:v2:" <> show (round metres :: Int) <> ":" <> show (round (p.lat * 1e5) :: Integer) <> ":" <> show (round (p.lon * 1e5) :: Integer)
  Redis.safeGet cacheKey >>= \case
    Just cached -> pure [(Maps.LatLong lat lon, name) | (lat, lon, name) <- cached]
    Nothing -> do
      res <- withTryCatch "shuttleRoutes:roadsNear" (Maps.getNearestPoints merchantOperatingCity.merchantId merchantOperatingCity.id Nothing (Maps.GetNearestPointsReq p (Just 5)))
      case res of
        Right resp -> do
          let found = [(w.location, mfilter (not . T.null) w.roadName) | w <- resp.waypoints, w.distance <= metres]
          -- "no road here" is not kept: it may only mean the road map does not reach here yet
          unless (null found) $ Redis.setExp cacheKey [(q.lat, q.lon, name) | (q, name) <- found] roadCacheSeconds
          pure found
        Left _ -> pure []

-- | Each road point's class from the road network itself, not its name: OSRM's travel time along the road for
-- classProbeMeters either way gives the speed the network holds for it (its road class: main roads are fast, lanes and
-- service roads slow). 3 a main road (mainRoadKmh and up), 2 an ordinary road (roadKmh and up), 1 a lane or side street.
classifyRoads :: DMOC.MerchantOperatingCity -> [(Maps.LatLong, Maybe Text)] -> Environment.Flow [RP.Site]
classifyRoads merchantOperatingCity points = do
  classed <- inParallel (\(p, name) -> (p,name,) . either (const Nothing) identity <$> withTryCatch "shuttleRoutes:roadClassCache" (Redis.safeGet (classKey p))) points
  let cached = [c | (_, _, c) <- classed]
      points' = [(p, name) | (p, name, _) <- classed]
      todo = [p | ((p, _), Nothing) <- zip points' cached]
      probes p = [offsetBy p classProbeMeters bearing | bearing <- [0, 90, 180, 270]]
      -- a few road points at a time, each to its own probes (a plain many-to-many table: stock OSRM has no one-to-one)
      batches = [(ps, concatMap probes ps) | ps <- chunksOf (pairChunk `div` 4) todo]
  answered <- concat <$> inParallel (uncurry table) batches
  let speeds =
        Map.fromListWith
          max
          [ (key6 p, meters / secs * 3.6)
            | ((p, q), (secs, meters)) <- answered,
              secs > 0,
              meters >= 0.4 * classProbeMeters,
              meters <= 1.4 * RP.metersBetween p q
          ]
      classOf kmh
        | kmh >= mainRoadKmh = RP.mainRoadClass
        | kmh >= roadKmh = 2
        | otherwise = 1
  void . withTryCatch "shuttleRoutes:roadClassSave" $ forM_ todo $ \p -> whenJust (Map.lookup (key6 p) speeds) $ \kmh -> Redis.setExp (classKey p) (classOf kmh) roadCacheSeconds
  pure [RP.Site p name (fromMaybe (maybe 1 classOf (Map.lookup (key6 p) speeds)) c) | ((p, name), c) <- zip points' cached]
  where
    key6 p = (round (p.lat * 1e6) :: Integer, round (p.lon * 1e6) :: Integer)
    classKey p = "CorporateShuttle:RoadClass:v2:" <> show (round (p.lat * 1e5) :: Integer) <> ":" <> show (round (p.lon * 1e5) :: Integer)
    -- only each road point's own probes count: a probe is within classProbeMeters (and a bit) of its road point
    table origins destinations = case (origins, destinations) of
      (o : os, d : ds) -> do
        res <- withTryCatch "shuttleRoutes:roadClass" (Maps.getOsrmDistances merchantOperatingCity.merchantId merchantOperatingCity.id Nothing (Maps.GetDistancesReq (o :| os) (d :| ds) Meter Nothing (Just Maps.CAR)))
        pure $ either (const []) (\rs -> [((r.origin, r.destination), (fromIntegral r.duration.getSeconds :: Double, fromIntegral r.distance.getMeters)) | r <- NE.toList rs, RP.metersBetween r.origin r.destination <= classProbeMeters * 1.05]) res
      _ -> pure []

-- | Road data barely changes: a week.
roadCacheSeconds :: Redis.ExpirationTime
roadCacheSeconds = 7 * 24 * 3600

-- | The point metres away on a compass bearing (degrees).
offsetBy :: Maps.LatLong -> Double -> Double -> Maps.LatLong
offsetBy p meters bearing =
  let b = bearing * pi / 180
   in Maps.LatLong (p.lat + meters * cos b / 111000) (p.lon + meters * sin b / (111000 * max 0.2 (cos (p.lat * pi / 180))))

-- | Someone no shared point suits walks to the nearest metro, bus stand or main road within a kilometre; with none
-- known, the main roads OSRM finds right by their home. None at all: Nothing (they are left off). Never an error.
ownStop :: DMOC.MerchantOperatingCity -> [RP.Site] -> Maps.LatLong -> Environment.Flow (Maybe RP.Site)
ownStop merchantOperatingCity sites p =
  case sortOn (\s -> RP.walkEstimate p s.siteAt) [s | s <- sites, RP.walkEstimate p s.siteAt <= RP.walkMeters] of
    s : _ -> pure (Just s)
    [] -> do
      near <- roadsNear merchantOperatingCity p
      classified <- classifyRoads merchantOperatingCity near
      pure $ listToMaybe (sortOn (RP.metersBetween p . (.siteAt)) [s | s <- classified, s.siteClass >= RP.mainRoadClass])

-- | Walking metres from each home to the pickup point along the streets, by home spot (either way round: a one-way street
-- does not bind a pedestrian). A home the maps service gives nothing for is left out (its estimate stands).
walkingMeters :: DMOC.MerchantOperatingCity -> [Maps.LatLong] -> Maps.LatLong -> Environment.Flow (Map.Map (Integer, Integer) Double)
walkingMeters merchantOperatingCity homesAt site = do
  there <- concat <$> forM (chunksOf matrixChunk homesAt) (\chunk -> distances chunk [site])
  let far = [o | ((o, _), (_, m)) <- there, m > RP.walkMeters]
  back <- concat <$> forM (chunksOf matrixChunk far) (distances [site])
  pure $ Map.fromListWith min ([(spot o, m) | ((o, _), (_, m)) <- there] <> [(spot d, m) | ((_, d), (_, m)) <- back])
  where
    spot p = (round (p.lat * 1e4) :: Integer, round (p.lon * 1e4) :: Integer)
    distances os ds = roadDistances merchantOperatingCity Maps.FOOT os ds

-- | Road distances between points, both as given: ((origin, destination), (seconds, metres)); none when the maps service
-- cannot answer.
roadDistances :: DMOC.MerchantOperatingCity -> Maps.TravelMode -> [Maps.LatLong] -> [Maps.LatLong] -> Environment.Flow [((Maps.LatLong, Maps.LatLong), (Double, Double))]
roadDistances merchantOperatingCity mode origins destinations = case (origins, destinations) of
  (o : os, d : ds) -> do
    res <- withTryCatch "shuttleRoutes:distances" (Maps.getDistances merchantOperatingCity.merchantId merchantOperatingCity.id Nothing (Maps.GetDistancesReq (o :| os) (d :| ds) Meter Nothing (Just mode)))
    pure $ either (const []) (\rs -> [((r.origin, r.destination), (fromIntegral r.duration.getSeconds, fromIntegral r.distance.getMeters)) | r <- NE.toList rs]) res
  _ -> pure []

-- | Road travel between every two nodes, each way, from the maps service's matrix (one-way streets, bridges and signals
-- included); a pair it cannot answer is estimated from the straight line (1.4 times it, at 20 km/h).
roadMatrix :: DMOC.MerchantOperatingCity -> [Maps.LatLong] -> Environment.Flow RP.Matrix
roadMatrix merchantOperatingCity nodes = do
  let indexed = zip [0 :: Int ..] nodes
      key p = (round (p.lat * 1e6) :: Integer, round (p.lon * 1e6) :: Integer)
      indexOf = Map.fromList [(key p, i) | (i, p) <- reverse indexed]
      chunks = chunksOf matrixChunk nodes
  answered <- concat <$> inParallel (\(os, ds) -> roadDistances merchantOperatingCity Maps.CAR os ds) [(os, ds) | os <- chunks, ds <- chunks]
  let known = Map.fromList [((i, j), v) | ((o, d), v) <- answered, Just i <- [Map.lookup (key o) indexOf], Just j <- [Map.lookup (key d) indexOf], i /= j]
      estimate a b = let m = RP.metersBetween a b * 1.4 in (m / 1000 / 20 * 3600, m)
      full = Map.fromList [((i, j), Map.findWithDefault (estimate a b) (i, j) known) | (i, a) <- indexed, (j, b) <- indexed, i /= j]
  pure RP.Matrix {RP.travelSecs = Map.map fst full, RP.travelMeters = Map.map snd full}

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = case splitAt n xs of
  ([], _) -> []
  (c, rest) -> c : chunksOf n rest

-- | Drive the stops in order along the roads: (the stops driven, each with whether it was moved; for each given point
-- the stop it became; minutes from the first stop at each stop; road line; metres). The shuttle never drives in and back
-- out the same way: a pickup it would reach down a road and leave by that road back moves to where that road leaves the
-- route (its riders walk the stretch, up to walkMeters), and pickups that end up together are one stop. Only stops marked
-- movable (the pickups, not the office) move, and never the first or last stop (the trip starts or ends there).
drive :: DMOC.MerchantOperatingCity -> [Bool] -> [Maps.LatLong] -> Environment.Flow ([(Maps.LatLong, Bool)], [Int], [Int], [[Double]], Int)
drive merchantOperatingCity movable points0 = do
  placed <- onThroughPath
  (stops, legs) <- noSpurs (4 :: Int) [Stop p wasMoved mv [i] | (i, ((p, wasMoved), mv)) <- zip [0 ..] (zip placed movable)]
  let offsets = scanl (\acc (secs, _, _) -> acc + max 1 (secs `div` 60) + 1) 0 legs
      shape = [[p.lat, p.lon] | p <- take 1 (map (.stopAt) stops) <> concatMap (\(_, _, ps) -> drop 1 ps) legs]
      stopOf = IntMap.fromList [(i, k) | (k, st) <- zip [0 ..] stops, i <- st.stopInputs]
  pure ([(st.stopAt, st.stopMoved) | st <- stops], [IntMap.findWithDefault 0 i stopOf | i <- [0 .. length points0 - 1]], offsets, shape, sum [m | (_, m, _) <- legs])
  where
    lastIx0 = length points0 - 1
    -- a pickup within throughPathMeters of the shuttle's own drive from the stop before to the stop after moves onto
    -- that drive when the drive there is a main road: its riders walk to it (crossing the road if they must), and the
    -- shuttle never turns off its way, into a side road or round a U-turn, to reach them
    onThroughPath = go [] (zip3 [0 :: Int ..] points0 movable)
      where
        go acc [] = pure (reverse acc)
        go acc ((k, p, mv) : rest) = case (acc, rest) of
          ((prev, _) : _, (_, next, _) : _) | mv && k > 0 && k < lastIx0 -> do
            line <- routeLine prev next
            case nearestOnLine p line of
              Just q | RP.metersBetween p q > 15 && RP.metersBetween p q <= throughPathMeters -> do
                main <- onMainRoad q
                go ((if main then (q, True) else (p, False)) : acc) rest
              _ -> go ((p, False) : acc) rest
          _ -> go ((p, False) : acc) rest
    onMainRoad q = any (\st -> st.siteClass >= RP.mainRoadClass) <$> classifyRoads merchantOperatingCity [(q, Nothing)]
    -- drive, find every stop the shuttle would reach and leave along the same road (looking at the whole drive before
    -- and after it), move it to where that road leaves the route, join stops that are then together; again, until the
    -- drive has none
    noSpurs n stops = do
      legs <- legsOf (map (.stopAt) stops)
      let paths = map (\(_, _, ps) -> ps) legs
          lastIx = length stops - 1
          entry k = do
            guard (k > 0 && k < lastIx)
            spurEntry (concat (take k paths)) (concat (drop k paths))
          moved = [if st.stopMovable then maybe st (\q -> st {stopAt = q, stopMoved = True}) (entry k) else st | (k, st) <- zip [0 :: Int ..] stops]
      if n <= 0 || map (.stopAt) moved == map (.stopAt) stops
        then pure (stops, legs)
        else noSpurs (n - 1 :: Int) (joinClose moved)
    -- a stop within mergeMeters of the stop before it is that stop (both pickups)
    joinClose =
      reverse
        . foldl
          ( \acc st -> case acc of
              prev : rest | prev.stopMovable && st.stopMovable && RP.metersBetween prev.stopAt st.stopAt <= mergeMeters -> prev {stopInputs = prev.stopInputs <> st.stopInputs} : rest
              _ -> st : acc
          )
          []
    routeLine a b = do
      res <- withTryCatch "shuttleRoutes:throughPath" (Maps.getRoutes Nothing (Id.Id "corporate-shuttle-planner") merchantOperatingCity.merchantId (Just merchantOperatingCity.id) Nothing (Maps.GetRoutesReq (a :| [b]) (Just Maps.CAR) True))
      pure $ case res of
        Right (r : _) | not (null r.points) -> r.points
        _ -> []
    legsOf pts = inParallel `flip` zip pts (drop 1 pts) $ \(a, b) -> do
      res <- withTryCatch "shuttleRoutes:route" (Maps.getRoutes Nothing (Id.Id "corporate-shuttle-planner") merchantOperatingCity.merchantId (Just merchantOperatingCity.id) Nothing (Maps.GetRoutesReq (a :| [b]) (Just Maps.CAR) True))
      pure $ case res of
        Right (r : _) | Just d <- r.duration, not (null r.points) -> (d.getSeconds, maybe (estimateMeters a b) (.getMeters) r.distance, r.points)
        _ -> (round (RP.metersBetween a b * 1.4 / 1000 / 20 * 3600), estimateMeters a b, [a, b])
    estimateMeters a b = round (RP.metersBetween a b * 1.4)

-- | A stop while a route is driven: where it is, whether it was moved there, whether it may move, and the given points it
-- serves (more than one when pickups were joined).
data Stop = Stop {stopAt :: Maps.LatLong, stopMoved :: Bool, stopMovable :: Bool, stopInputs :: [Int]}

-- | Pickups this close together on a route are one stop.
mergeMeters :: Double
mergeMeters = 150

-- | Where the shuttle would drive into a road for a stop and come back out the same way: the drive up to the stop (inPath,
-- ending at it) and after it (outPath, starting at it) share their stretch by the stop. The point where they part (at
-- most RP.walkMeters along the shared stretch: the riders walk it), when that shared stretch is at least spurMinMeters.
spurEntry :: [Maps.LatLong] -> [Maps.LatLong] -> Maybe Maps.LatLong
spurEntry inPath outPath =
  let back = reverse inPath
      shared = takeWhile (\q -> distanceToLine q back <= spurToleranceMeters) (drop 1 outPath)
      along = zip shared (drop 1 (scanl (+) 0 (zipWith RP.metersBetween (take 1 outPath <> shared) shared)))
      stretch = maybe 0 snd (lastMay along)
   in if stretch >= spurMinMeters then fst <$> lastMay (takeWhile ((<= RP.walkMeters) . snd) along) else Nothing

-- | How far a pickup may move onto the shuttle's own drive (its riders then walk that much further).
throughPathMeters :: Double
throughPathMeters = 500

-- | The point of a road line nearest to a point (on its nearest segment).
nearestOnLine :: Maps.LatLong -> [Maps.LatLong] -> Maybe Maps.LatLong
nearestOnLine q line = fmap snd (listToMaybe (sortOn fst [onSegment a b | (a, b) <- zip line (drop 1 line)]))
  where
    kx = 111320 * cos (q.lat * pi / 180)
    ky = 110540
    xy p = ((p.lon - q.lon) * kx, (p.lat - q.lat) * ky)
    onSegment a b =
      let (ax, ay) = xy a
          (bx, by) = xy b
          (dx, dy) = (bx - ax, by - ay)
          len2 = dx * dx + dy * dy
          t = if len2 == 0 then 0 else max 0 (min 1 (negate (ax * dx + ay * dy) / len2))
          (px, py) = (ax + t * dx, ay + t * dy)
       in (px * px + py * py, Maps.LatLong (q.lat + py / ky) (q.lon + px / kx))

-- | A stretch the shuttle would drive in and back out shorter than this is left alone (a U-turn at the stop's own corner).
spurMinMeters, spurToleranceMeters :: Double
spurMinMeters = 80
spurToleranceMeters = 20

-- | Metres from a point to a road line (its nearest segment).
distanceToLine :: Maps.LatLong -> [Maps.LatLong] -> Double
distanceToLine q line = minimum (1e9 : map (RP.metersBetween q) (take 1 line) <> [toSegment a b | (a, b) <- zip line (drop 1 line)])
  where
    -- on a local flat projection around q, in metres
    xy p = ((p.lon - q.lon) * 111320 * cos (q.lat * pi / 180), (p.lat - q.lat) * 110540)
    toSegment a b =
      let (ax, ay) = xy a
          (bx, by) = xy b
          (dx, dy) = (bx - ax, by - ay)
          len2 = dx * dx + dy * dy
          t = if len2 == 0 then 0 else max 0 (min 1 (negate (ax * dx + ay * dy) / len2))
          (px, py) = (ax + t * dx, ay + t * dy)
       in sqrt (px * px + py * py)

-- | The road (or the area) a stop is on, never a house number or an employee's address.
stopName :: DMOC.MerchantOperatingCity -> Maps.LatLong -> Environment.Flow Text
stopName merchantOperatingCity p =
  namedRoad >>= \case
    Just name -> pure name
    Nothing -> placeName
  where
    placeName = do
      res <- withTryCatch "shuttleRoutes:name" (Maps.getPlaceName merchantOperatingCity.merchantId merchantOperatingCity.id Nothing (Maps.GetPlaceNameReq (Maps.ByLatLong p) Nothing Nothing))
      let components = either (const []) (concatMap (.addressComponents)) res
          named kinds = listToMaybe [c.longName | c <- components, any (`elem` c.types) kinds]
      pure $ fromMaybe "Pickup point" (named ["route"])
    -- the nearest road with a name within nameMaxMeters (the stop itself snaps to a road within snapMaxMeters)
    namedRoad = do
      res <- withTryCatch "shuttleRoutes:roadName" (Maps.getNearestPoints merchantOperatingCity.merchantId merchantOperatingCity.id Nothing (Maps.GetNearestPointsReq p (Just 10)))
      pure $ either (const Nothing) (\resp -> listToMaybe [name | w <- resp.waypoints, w.distance <= nameMaxMeters, Just name <- [mfilter (not . T.null) w.roadName]]) res

-- | The nearest road to a point (the city's OSRM), when it is within snapMaxMeters: where a stop snaps to, and its name.
nearestRoad :: DMOC.MerchantOperatingCity -> Maps.LatLong -> Environment.Flow (Maybe Maps.NearestPoint)
nearestRoad merchantOperatingCity p = do
  res <- withTryCatch "shuttleRoutes:nearest" (Maps.getNearestPoints merchantOperatingCity.merchantId merchantOperatingCity.id Nothing (Maps.GetNearestPointsReq p (Just 5)))
  pure $ case res of
    -- the nearest road with a name (an unnamed lane says nothing to a rider), else the nearest road
    Right resp ->
      let near = filter (\w -> w.distance <= snapMaxMeters) resp.waypoints
       in find (isJust . mfilter (not . T.null) . (.roadName)) near <|> listToMaybe near
    Left _ -> Nothing

-- | The company's code in every id: its name's initials ("Namma Yatri" -> NY), or the whole name when it is one word
-- ("Juspay" -> JUSPAY); words such as Corp or Pvt Ltd are left out ("ACME Corp" -> ACME).
orgCode :: Text -> Text
orgCode companyName =
  let ws = filter ((`notElem` companyWords) . T.toLower) (T.words (T.map (\c -> if isAlphaNum c then c else ' ') companyName))
      companyWords = ["corp", "corporation", "inc", "ltd", "limited", "pvt", "private", "llc", "llp", "co", "company"]
   in T.toUpper (T.take 8 (if length ws > 1 then T.concat (map (T.take 1) ws) else T.concat ws))

routeName :: [PlannedStop] -> Text
routeName stops = T.intercalate " → " (map NE.head (NE.group (map (.name) stops)))

mkTripRow :: DIBC.IntegratedBPPConfig -> DMOC.MerchantOperatingCity -> UTCTime -> Text -> Text -> Text -> Int -> Int -> DShuttlePublishedTrip.ShuttlePublishedTrip
mkTripRow config merchantOperatingCity now routeCode tripId day dep riders =
  DShuttlePublishedTrip.ShuttlePublishedTrip
    { tripId,
      day,
      feedKey = config.feedKey,
      routeCode,
      departure = minutesToHHMM dep,
      riders,
      merchantId = merchantOperatingCity.merchantId,
      merchantOperatingCityId = merchantOperatingCity.id,
      createdAt = now,
      updatedAt = now
    }

tripIdOf :: Text -> Int -> Text
tripIdOf routeCode dep = routeCode <> "-" <> T.filter (/= ':') (minutesToHHMM dep)

-- | These routes' trips become the given rows: a row with the same trip and day is updated, rows no longer given are
-- removed, the rest created.
replaceTrips :: DIBC.IntegratedBPPConfig -> [Text] -> [DShuttlePublishedTrip.ShuttlePublishedTrip] -> [DShuttlePublishedTrip.ShuttlePublishedTrip] -> Environment.Flow ()
replaceTrips config routeCodes timetable rows = do
  let tripKey t = (t.tripId, t.day)
      current = filter (\t -> t.routeCode `elem` routeCodes) timetable
  forM_ (filter (\t -> tripKey t `notElem` map tripKey rows) current) $ \t -> QShuttlePublishedTrip.deleteByFeedKeyAndTripIdAndDay config.feedKey t.tripId t.day
  forM_ rows $ \t -> if tripKey t `elem` map tripKey current then QShuttlePublishedTrip.updateByPrimaryKey t else QShuttlePublishedTrip.create t

-- | Minutes after the service day's midnight -> "HH:MM"; past midnight stays on that service day (24:10, as GTFS writes it).
minutesToHHMM :: Int -> Text
minutesToHHMM m = pad (m `div` 60) <> ":" <> pad (m `mod` 60)
  where
    pad n = T.justifyRight 2 '0' (show n)

parseHHMM :: Text -> Maybe Int
parseHHMM t = case T.splitOn ":" t of
  [h, m] | T.length h == 2, T.length m == 2, Just hh <- readMaybe (T.unpack h), Just mm <- readMaybe (T.unpack m), hh < 48, mm < 60 -> Just (hh * 60 + mm)
  _ -> Nothing

-- Publishing ----------------------------------------------------------------------------------------------------------

-- | Plan the city's routes from the interest forms and give them as the city's GTFS feed (corp_<city>), to load into the
-- GIMS editor: a new feed from the zip the first time, a reload of the feed after that. Saves what GIMS does not keep:
-- each trip's days and riders (shuttle_published_trip, which the run engine reads) and the routes' seat fares.
postShuttleInterestShuttleRoutesPublish :: Id.ShortId DMerchant.Merchant -> Context.City -> Environment.Flow ShuttleInterestAPI.ShuttleRoutesPublishResp
postShuttleInterestShuttleRoutesPublish merchantShortId opCity = do
  merchantOperatingCity <- findCity merchantShortId opCity
  config <- requireCorpConfig merchantOperatingCity
  published <- filter (not . null . snd) <$> planCity config merchantOperatingCity
  when (null published) $ throwError $ InvalidRequest "There are no routes to publish yet: no interest forms, or no company office set"
  forM_ published $ \(r, _) -> do
    let times = map (.minutesFromStart) r.stops
    unless (length r.stops >= 2) $ throwError $ InvalidRequest (r.routeCode <> " needs at least 2 stops")
    unless (take 1 times == [0] && and (zipWith (<) times (drop 1 times))) $ throwError $ InvalidRequest (r.routeCode <> ": the stop times must increase")
    unless (r.fareAmount > 0) $ throwError $ InvalidRequest (r.routeCode <> " needs a fare")
  tier <- QFRFSVehicleServiceTier.findAllByMerchantOperatingCityIdAndIntegratedBPPConfigId merchantOperatingCity.id config.id >>= fromMaybeM (InvalidRequest "The corporate shuttle feed has no vehicle service tier") . listToMaybe
  now <- getCurrentTime
  forM_ published $ \(r, _) -> writeFares config merchantOperatingCity tier now r
  -- the published timetable becomes this plan's (rows still planned are updated in place, the rest removed: never deleted
  -- and re-created under one key, which the KV drainer could apply out of order)
  -- one publish of a feed at a time: a second waits, so the stored timetable is always one plan's
  Redis.withWaitAndLockRedis ("CorporateShuttle:Publish:Lock:" <> config.feedKey) 60 100000 $ do
    timetable <- QShuttlePublishedTrip.findAllByFeedKeyAndMerchantOperatingCityId config.feedKey merchantOperatingCity.id
    replaceTrips config (nubOrd (map (.routeCode) timetable <> map ((.routeCode) . fst) published)) timetable (concatMap snd published)
  let city = T.drop 5 config.feedKey
      folder = city <> ".corp_shuttle.gtfs"
      files = gtfsFiles config (utctDay now) published
      archive = foldr (\(name, body) -> Zip.addEntryToArchive (Zip.toEntry (T.unpack (folder <> "/" <> name)) 315532800 (BL.fromStrict (TE.encodeUtf8 body)))) Zip.emptyArchive files
  pure ShuttleInterestAPI.ShuttleRoutesPublishResp {ShuttleInterestAPI.fileName = folder <> ".zip", ShuttleInterestAPI.gtfsZipBase64 = TE.decodeUtf8 (B64.encode (BL.toStrict (Zip.fromArchive archive)))}

-- | Seat fares for every forward stop pair of the route, as the FRFS fare tables read them (one fare policy per route).
writeFares :: DIBC.IntegratedBPPConfig -> DMOC.MerchantOperatingCity -> DFRFSVehicleServiceTier.FRFSVehicleServiceTier -> UTCTime -> PlannedRoute -> Environment.Flow ()
writeFares config merchantOperatingCity tier now route = do
  farePolicyId <-
    QFRFSRouteFareProduct.findByRouteCode route.routeCode config.id >>= \case
      (fareProduct : _) -> pure fareProduct.farePolicyId
      [] -> do
        farePolicyId <- Id.Id <$> generateGUID
        QFRFSFarePolicy.create DFRFSFarePolicy.FRFSFarePolicy {_type = DFRFSFarePolicy.MatrixBased, applicableDiscountIds = [], cessCharge = Nothing, description = "corporate shuttle seat fare " <> route.routeCode, id = farePolicyId, merchantId = merchantOperatingCity.merchantId, merchantOperatingCityId = merchantOperatingCity.id, createdAt = now, updatedAt = now}
        productId <- Id.Id <$> generateGUID
        QFRFSRouteFareProduct.create DFRFSRouteFareProduct.FRFSRouteFareProduct {farePolicyId, id = productId, integratedBppConfigId = config.id, merchantId = merchantOperatingCity.merchantId, merchantOperatingCityId = merchantOperatingCity.id, routeCode = route.routeCode, timeBounds = Unbounded, vehicleServiceTierId = tier.id, vehicleType = FRFSEnums.BUS, createdAt = now, updatedAt = now}
        pure farePolicyId
  let codes = map (.stopCode) route.stops
  forM_ (nubOrd [(a, b) | (i, a) <- zip [0 :: Int ..] codes, b <- drop (i + 1) codes, a /= b]) $ \(start, end) ->
    QStopFare.findByPrimaryKey ADULT end farePolicyId start >>= \case
      Just fare -> QStopFare.updateByPrimaryKey fare {DStopFare.amount = route.fareAmount, DStopFare.updatedAt = now}
      Nothing -> QStopFare.create DStopFare.StopFare {amount = route.fareAmount, bppItemId = Nothing, category = ADULT, currency = INR, endStopCode = end, farePolicyId, integratedBppConfigId = config.id, merchantId = merchantOperatingCity.merchantId, merchantOperatingCityId = merchantOperatingCity.id, offeredAmount = Nothing, startStopCode = start, createdAt = now, updatedAt = now}

-- | The feed's files: feed_info, agency, stops, routes, trips, stop_times, calendar and shapes. A trip runs on the days its
-- timetable rows give, a year from today; one GTFS service per distinct set of days.
gtfsFiles :: DIBC.IntegratedBPPConfig -> Day -> [(PlannedRoute, [DShuttlePublishedTrip.ShuttlePublishedTrip])] -> [(Text, Text)]
gtfsFiles config today published =
  [ ("feed_info.txt", csv ["feed_publisher_name", "feed_publisher_url", "feed_lang", "feed_start_date", "feed_end_date", "feed_version", "feed_id"] [["Namma Yatri", "https://nammayatri.in", "en", start, end, start, config.feedKey]]),
    ("agency.txt", csv ["agency_id", "agency_name", "agency_url", "agency_timezone", "agency_lang"] [[config.feedKey, "Namma Yatri Corporate Shuttle", "https://nammayatri.in", "Asia/Kolkata", "en"]]),
    ("stops.txt", csv ["stop_id", "stop_code", "stop_name", "stop_lat", "stop_lon", "location_type"] [[s.stopCode, s.stopCode, s.name, show s.lat, show s.lon, "0"] | s <- stops]),
    ("routes.txt", csv ["route_id", "agency_id", "route_short_name", "route_long_name", "route_type"] [[r.routeCode, config.feedKey, T.replace "-" " " (T.drop 5 r.routeCode), r.name, "3"] | (r, _) <- published]),
    ("trips.txt", csv ["route_id", "service_id", "trip_id", "direction_id", "trip_headsign", "shape_id"] [[r.routeCode, serviceOf days, tid, "0", maybe "" (.name) (lastMay r.stops), r.routeCode] | (r, tid, _, days) <- trips]),
    ("stop_times.txt", csv ["trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "timepoint"] [[tid, gtfsTime (dep + s.minutesFromStart), gtfsTime (dep + s.minutesFromStart), s.stopCode, show (i :: Int), "1"] | (r, tid, dep, _) <- trips, (i, s) <- zip [1 ..] r.stops]),
    ("calendar.txt", csv (["service_id"] <> weekDays <> ["start_date", "end_date"]) [[serviceOf days] <> [if d `elem` days then "1" else "0" | d <- weekDays] <> [start, end] | days <- nubOrd [days | (_, _, _, days) <- trips]]),
    ("shapes.txt", csv ["shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"] [[r.routeCode, show lat, show lon, show (i :: Int)] | (r, _) <- published, (i, [lat, lon]) <- zip [1 ..] r.shape])
  ]
  where
    start = T.pack (formatTime defaultTimeLocale "%Y%m%d" today)
    end = T.pack (formatTime defaultTimeLocale "%Y%m%d" (addDays 365 today))
    stops = Map.elems (Map.fromList [(s.stopCode, s) | (r, _) <- published, s <- r.stops])
    trips =
      [ (r, tid, fromMaybe 0 (parseHHMM (maybe "" (.departure) (listToMaybe rows))), sortOn dayIndex (map (.day) rows))
        | (r, ts) <- published,
          (tid, rows) <- Map.toList (Map.fromListWith (<>) [(t.tripId, [t]) | t <- ts])
      ]
    serviceOf days = T.intercalate "_" (map (T.toUpper . T.take 3) days) <> "_" <> start <> "_" <> end
    gtfsTime m = T.justifyRight 2 '0' (show (m `div` 60)) <> ":" <> T.justifyRight 2 '0' (show (m `mod` 60)) <> ":00"
    csv header rows = T.unlines (map (T.intercalate "," . map field) (header : rows))
    field f = if T.any (`elem` [',', '"', '\n']) f then "\"" <> T.replace "\"" "\"\"" f <> "\"" else f
