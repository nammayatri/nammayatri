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
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
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
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Id as Id
import Kernel.Types.TimeBound (TimeBound (Unbounded))
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common (Currency (INR), DistanceUnit (Meter), HighPrecMoney, Seconds (..), fromMaybeM, generateGUID, getCurrentTime, throwError, withTryCatch)
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
  pure $ ShuttleInterestAPI.ShuttleInterestMapResp {ShuttleInterestAPI.points = map mkMapPoint interests}

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

mkMapPoint :: DShuttleInterest.ShuttleInterest -> ShuttleInterestAPI.ShuttleInterestMapPoint
mkMapPoint interest =
  ShuttleInterestAPI.ShuttleInterestMapPoint
    { ShuttleInterestAPI.personId = interest.personId,
      ShuttleInterestAPI.passOrganizationId = interest.passOrganizationId,
      ShuttleInterestAPI.companyDomain = interest.companyDomain,
      ShuttleInterestAPI.tripType = interest.tripType,
      ShuttleInterestAPI.pickupLat = interest.pickupLat,
      ShuttleInterestAPI.pickupLon = interest.pickupLon,
      ShuttleInterestAPI.dropLat = interest.dropLat,
      ShuttleInterestAPI.dropLon = interest.dropLon
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
    trips = mkTrips timetable
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
mkTrips :: [DShuttlePublishedTrip.ShuttlePublishedTrip] -> [ShuttleInterestAPI.ShuttlePublishedTripInfo]
mkTrips rows =
  [ ShuttleInterestAPI.ShuttlePublishedTripInfo
      { ShuttleInterestAPI.tripId = tripId,
        ShuttleInterestAPI.departure = maybe "" (.departure) (listToMaybe tripRows),
        ShuttleInterestAPI.days = sortOn dayIndex (map (.day) tripRows),
        ShuttleInterestAPI.riders = maximum (0 : map (.riders) tripRows)
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
-- From one company's interest forms, per direction (MORNING to the office, EVENING home):
--   pickup points  everyone within walkMeters (500 m) of a point shares it, whatever their time (greedy cover over the
--                  employees' own pickup points, the point the most still-unplaced employees can walk to first)
--   routes         points within linkMeters (3 km) of each other, directly or through other points, are one route, picked
--                  up farthest first and ending at the office (EVENING: the reverse); planned once from every day's riders,
--                  so a route has the same stops every day
--   shuttles       per route and day, riders with the same time (to 15 minutes) share a shuttle: 8 seats, at least 5 meant;
--                  a full time is split into shuttles 5 minutes apart, a light one joins the nearest time within 30 minutes
--                  when both fit; nobody is dropped, so a shuttle that stays light still runs
--   roads          stops snap to the road and the legs and line come from the city's maps service (straight-line estimate
--                  when it cannot answer); a stop is named after its road, never an employee's address

walkMeters, linkMeters, snapMaxMeters, nameMaxMeters :: Double
walkMeters = 500
linkMeters = 3000
snapMaxMeters = 200
nameMaxMeters = 500

slotMinutes, seats, minLoad, mergeMinutes, splitGapMinutes :: Int
slotMinutes = 15
seats = 8
minLoad = 5
mergeMinutes = 30
splitGapMinutes = 5

weekDays :: [Text]
weekDays = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]

dayIndex :: Text -> Int
dayIndex d = fromMaybe 7 (elemIndex d weekDays)

data Rider = Rider {riderId :: Text, home :: Maps.LatLong, wish :: Int, riderDays :: [Text]}

-- | A stop while planning: where it is, and who boards there (MORNING) or gets off there (EVENING).
data PlanStop = PlanStop {at :: Maps.LatLong, office :: Bool, boarders :: [Text]}

metersBetween :: Maps.LatLong -> Maps.LatLong -> Double
metersBetween a b = realToFrac (distanceBetweenInMeters a b)

-- | A planned route: one company's, in the city's corp feed.
data PlannedRoute = PlannedRoute {routeCode :: Text, passOrganizationId :: Id.Id DPassOrganization.PassOrganization, name :: Text, fareAmount :: HighPrecMoney, stops :: [PlannedStop], shape :: [[Double]]}

data PlannedStop = PlannedStop {stopCode :: Text, name :: Text, lat :: Double, lon :: Double, minutesFromStart :: Int}

-- | Every company's routes in the city, each with its trips (one row per trip and day), planned from the interest forms.
-- A company without an office location has no routes yet.
planCity :: DIBC.IntegratedBPPConfig -> DMOC.MerchantOperatingCity -> Environment.Flow [(PlannedRoute, [DShuttlePublishedTrip.ShuttlePublishedTrip])]
planCity config merchantOperatingCity = do
  companies <- QPassOrganization.findByMerchantOperatingCityIdAndPassEnum merchantOperatingCity.id DPassType.RegularPass
  let corporate = filter (\c -> not (null c.emailDomains)) companies
  forM_ corporate $ \c ->
    when (any (\o -> o.id /= c.id && orgCode o.name == orgCode c.name) corporate) $
      throwError $ InvalidRequest ("Two companies in this city get the same route code " <> orgCode c.name <> "; rename one of them")
  concat <$> mapM (planCompany config merchantOperatingCity) corporate

planCompany :: DIBC.IntegratedBPPConfig -> DMOC.MerchantOperatingCity -> DPassOrganization.PassOrganization -> Environment.Flow [(PlannedRoute, [DShuttlePublishedTrip.ShuttlePublishedTrip])]
planCompany config merchantOperatingCity company = case (company.officeLat, company.officeLon) of
  (Just lat, Just lon) -> do
    let officeAt = Maps.LatLong lat lon
        org = orgCode company.name
    interests <- allInterests merchantOperatingCity company
    laidOut <- forM [True, False] $ \morning -> do
      let everyone = ridersFor morning Nothing interests
      forM (layout officeAt everyone) $ \pickups -> do
        let ordered = if morning then pickups else reverse pickups
        (points, offsets, shape) <- onRoads merchantOperatingCity (map (.at) ordered)
        let stops = zipWith (\s p -> s {at = p}) ordered points
            stopAt = Map.fromList [(pid, i) | (i, s) <- zip [0 ..] stops, pid <- s.boarders]
            trips =
              [ (day, dep, ids)
                | day <- weekDays,
                  (t, ids) <- shuttles [(r.wish, r.riderId) | r <- ridersFor morning (Just day) interests, Map.member r.riderId stopAt],
                  -- MORNING: at the first stop with riders of this shuttle at their time; EVENING: from the office then
                  -- a service day starts at midnight: a shuttle would leave before it leaves at 00:00 instead
                  let dep = max 0 (if morning then t - minimum [offsets !! (stopAt Map.! pid) | pid <- ids] else t)
              ]
        pure (morning, stops, offsets, shape, uniqueDepartures trips)
    let planned = sortOn (\(morning, _, _, _, trips) -> (not morning, minimum (1440 : [d | (_, d, _) <- trips]))) (concat laidOut)
        stopCodes = Map.fromList (zip (nubOrd [key s.at | (_, stops, _, _, _) <- planned, s <- stops]) [1 :: Int ..])
        key p = (round (p.lat * 1e6) :: Integer, round (p.lon * 1e6) :: Integer)
    now <- getCurrentTime
    forM (zip [1 :: Int ..] planned) $ \(n, (_, stops, offsets, shape, trips)) -> do
      let routeCode = "CORP-" <> org <> "-" <> show n
      named <- forM (zip stops offsets) $ \(s, offset) -> do
        name <- if s.office then pure (company.name <> " office") else stopName merchantOperatingCity s.at
        pure PlannedStop {stopCode = "CORP_" <> org <> "_S" <> show (stopCodes Map.! key s.at), name, lat = s.at.lat, lon = s.at.lon, minutesFromStart = offset}
      pure
        ( PlannedRoute {routeCode, passOrganizationId = company.id, name = routeName named, fareAmount = defaultFare, stops = named, shape},
          [mkTripRow config merchantOperatingCity now routeCode day dep (length ids) | (day, dep, ids) <- trips]
        )
  _ -> pure []
  where
    defaultFare = 40
    -- two shuttles of one route never leave at the same minute on a day: a trip id is <route>-<HHMM>
    uniqueDepartures = concatMap bump . groupBy' (\(d, _, _) -> d)
    bump dayTrips = snd (mapAccumL (\used (d, dep, ids) -> let dep' = until (`notElem` used) (+ splitGapMinutes) dep in (dep' : used, (d, dep', ids))) [] dayTrips)
    groupBy' f = Map.elems . Map.fromListWith (flip (<>)) . map (\x -> (f x, [x]))

-- | Every current interest form of the company, read page by page (none is left out of the plan).
allInterests :: DMOC.MerchantOperatingCity -> DPassOrganization.PassOrganization -> Environment.Flow [DShuttleInterest.ShuttleInterest]
allInterests merchantOperatingCity company = go 0
  where
    pageSize = 1000
    go offset = do
      page <- QShuttleInterest.findAllByMerchantOperatingCityIdAndPassOrganizationId (Just pageSize) (Just offset) merchantOperatingCity.id company.id
      if length page < pageSize then pure page else (page <>) <$> go (offset + pageSize)

-- | That direction's riders (on that day, or on any day), each with the time they want, in minutes after midnight.
ridersFor :: Bool -> Maybe Text -> [DShuttleInterest.ShuttleInterest] -> [Rider]
ridersFor morning mbDay interests =
  sortOn (.riderId) $
    [ Rider {riderId = interest.personId.getId, home = Maps.LatLong interest.pickupLat interest.pickupLon, wish = todHour t * 60 + todMin t, riderDays = days}
      | interest <- interests,
        interest.tripType /= (if morning then DShuttleInterest.TO_HOME else DShuttleInterest.TO_OFFICE),
        let days = map (T.toLower . show) interest.daysOfWeek,
        maybe (not (null days)) (`elem` days) mbDay,
        Just t <- [if morning then interest.pickupTime else interest.dropTime]
    ]

-- | Routes in MORNING order (pickups farthest first, then the office) that carry every rider.
layout :: Maps.LatLong -> [Rider] -> [[PlanStop]]
layout officeAt riders = [sortOn (\s -> negate (metersBetween s.at officeAt)) group <> [PlanStop officeAt True []] | group <- link (cover riders)]

cover :: [Rider] -> [PlanStop]
cover riders = go (IntMap.map (sum . map weight) near) (IntSet.fromList (IntMap.keys points))
  where
    -- riders at the same spot (to about a metre) are one point weighted by its riders, so a crowded pickup costs one entry,
    -- not one per pair of riders; points keep the riders' order (the first such rider wins a tie)
    spot p = (round (p.lat * 1e5) :: Integer, round (p.lon * 1e5) :: Integer)
    points = IntMap.fromList (zip [0 ..] (sortOn ((.riderId) . NE.head) (Map.elems (Map.fromListWith (flip (<>)) [(spot r.home, r :| []) | r <- riders]))))
    homeOf i = (.home) (NE.head (points IntMap.! i))
    weight i = length (points IntMap.! i)
    -- each point's neighbourhood (points within walkMeters, itself included), looked for only in the grid cells around it:
    -- a cell is at least walkMeters wide, so no neighbour is missed
    cell p = (floor (p.lat / cellDegrees) :: Integer, floor (p.lon / cellDegrees) :: Integer)
    cellDegrees = 0.006
    byCell = Map.fromListWith (<>) [(cell (homeOf i), [i]) | i <- IntMap.keys points]
    near = IntMap.mapWithKey (\i _ -> let (y, x) = cell (homeOf i) in [j | dy <- [-1, 0, 1], dx <- [-1, 0, 1], j <- Map.findWithDefault [] (y + dy, x + dx) byCell, metersBetween (homeOf i) (homeOf j) <= walkMeters]) points
    go counts unplaced
      | IntSet.null unplaced = []
      | otherwise =
        -- the point the most still-unplaced riders can walk to
        let best = fst (IntMap.foldlWithKey (\b i c -> if c > snd b then (i, c) else b) (-1, 0) counts)
            taken = filter (`IntSet.member` unplaced) (near IntMap.! best)
            counts' = foldl (\m j -> foldl (flip (IntMap.adjust (subtract (weight j)))) m (near IntMap.! j)) counts taken
         in PlanStop (homeOf best) False (concatMap (map (.riderId) . NE.toList . (points IntMap.!)) taken) : go counts' (foldr IntSet.delete unplaced taken)

link :: [PlanStop] -> [[PlanStop]]
link = foldl add []
  where
    add groups s =
      let (near, far) = partition (any (\t -> metersBetween s.at t.at <= linkMeters)) groups
       in far <> [concat near <> [s]]

-- | One route's shuttles on one day: [(time, rider ids)] from (wanted time, rider id) pairs.
shuttles :: [(Int, Text)] -> [(Int, [Text])]
shuttles wishes = merge (concatMap split (Map.toList slots))
  where
    slots = Map.fromListWith (flip (<>)) [(roundTo slotMinutes w, [pid]) | (w, pid) <- sortOn identity wishes]
    roundTo k w = k * round (fromIntegral w / fromIntegral k :: Double)
    split (t, ids) =
      let n = (length ids + seats - 1) `div` seats
          size = (length ids + n - 1) `div` n
       in [(t + k * splitGapMinutes, take size (drop (k * size) ids)) | k <- [0 .. n - 1]]
    merge groups = maybe groups merge (listToMaybe (mapMaybe (tryMerge groups) [0 .. length groups - 1]))
    tryMerge groups i = do
      let (t, ids) = groups !! i
      guard (length ids < minLoad)
      -- any shuttle within mergeMinutes with room, the nearest in time first
      let fits j = j /= i && abs (fst (groups !! j) - t) <= mergeMinutes && length (snd (groups !! j)) + length ids <= seats
      j <- listToMaybe (sortOn (\j -> abs (fst (groups !! j) - t)) (filter fits [0 .. length groups - 1]))
      let (tj, ij) = groups !! j
          merged = (if length ij >= length ids then tj else t, ij <> ids)
      pure (sortOn fst (merged : [g | (k, g) <- zip [0 ..] groups, k /= i, k /= j]))

-- | Snap the stops to the road and drive them in order: (stops on the road, minutes from the first stop, road line).
onRoads :: DMOC.MerchantOperatingCity -> [Maps.LatLong] -> Environment.Flow ([Maps.LatLong], [Int], [[Double]])
onRoads merchantOperatingCity points = do
  snapped <- forM points $ \p ->
    nearestRoad merchantOperatingCity p >>= \case
      Just road -> pure road.location
      Nothing -> do
        res <- withTryCatch "shuttleRoutes:snap" (Maps.snapToRoad merchantOperatingCity.merchantId merchantOperatingCity.id Nothing (Maps.SnapToRoadReq [p] Meter Nothing))
        pure $ case res of
          Right r | Just s <- listToMaybe r.snappedPoints, metersBetween p s <= snapMaxMeters -> s
          _ -> p
  legs <- forM (zip snapped (drop 1 snapped)) $ \(a, b) -> do
    res <- withTryCatch "shuttleRoutes:route" (Maps.getRoutes Nothing (Id.Id "corporate-shuttle-planner") merchantOperatingCity.merchantId (Just merchantOperatingCity.id) Nothing (Maps.GetRoutesReq (a :| [b]) (Just Maps.CAR) True))
    pure $ case res of
      Right (r : _) | Just d <- r.duration, not (null r.points) -> (d.getSeconds, r.points)
      _ -> (round (metersBetween a b * 1.4 / 1000 / 20 * 3600), [a, b])
  let offsets = scanl (\acc (secs, _) -> acc + max 1 (secs `div` 60) + 1) 0 legs
      shape = [[p.lat, p.lon] | p <- take 1 snapped <> concatMap (drop 1 . snd) legs]
  pure (snapped, offsets, shape)

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

mkTripRow :: DIBC.IntegratedBPPConfig -> DMOC.MerchantOperatingCity -> UTCTime -> Text -> Text -> Int -> Int -> DShuttlePublishedTrip.ShuttlePublishedTrip
mkTripRow config merchantOperatingCity now routeCode day dep riders =
  DShuttlePublishedTrip.ShuttlePublishedTrip
    { tripId = routeCode <> "-" <> T.filter (/= ':') (minutesToHHMM dep),
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
  forM_ [(a, b) | (i, a) <- zip [0 :: Int ..] codes, b <- drop (i + 1) codes] $ \(start, end) ->
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
