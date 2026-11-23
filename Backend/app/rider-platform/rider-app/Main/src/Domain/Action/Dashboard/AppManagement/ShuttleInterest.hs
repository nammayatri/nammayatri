module Domain.Action.Dashboard.AppManagement.ShuttleInterest
  ( getShuttleInterestList,
    getShuttleInterestPublishedRoutes,
    getShuttleInterestMapPoints,
  )
where

import qualified API.Types.Dashboard.AppManagement.ShuttleInterest as ShuttleInterestAPI
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified "this" Domain.Types.PassOrganization as DPassOrganization
import qualified Domain.Types.PassType as DPassType
import qualified "this" Domain.Types.Person as DPerson
import qualified Domain.Types.Route as DRoute
import qualified Domain.Types.RouteStopMapping as DRouteStopMapping
import qualified "this" Domain.Types.ShuttleInterest as DShuttleInterest
import qualified Domain.Types.ShuttlePublishedTrip as DShuttlePublishedTrip
import qualified Environment
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (fromMaybeM)
import qualified SharedLogic.External.Nandi.Flow as NandiFlow
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.MerchantOperatingCity as QMerchantOperatingCity
import qualified Storage.Queries.PassOrganization as QPassOrganization
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ShuttleInterest as QShuttleInterest
import qualified Storage.Queries.ShuttlePublishedTrip as QShuttlePublishedTrip
import Tools.Error
import qualified Tools.MultiModal as MM

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

-- | The city's published corporate shuttle routes, read from GIMS: the corp_<city> GTFS feed, the single source for routes
-- (what riders and the run engine see). Each route with its stops in order, its first trip's times, and its trips with the
-- days they run (the published timetable, shuttle_published_trip: GIMS does not keep the trips' calendar). With a day
-- (e.g. monday), only the routes that run that day, with that day's trips and riders.
getShuttleInterestPublishedRoutes :: Id.ShortId DMerchant.Merchant -> Context.City -> Maybe Text -> Environment.Flow ShuttleInterestAPI.ShuttlePublishedRoutesResp
getShuttleInterestPublishedRoutes merchantShortId opCity mbDay = do
  merchantOperatingCity <- findCity merchantShortId opCity
  configs <- SIBC.findAllIntegratedBPPConfig merchantOperatingCity.id BecknSpec.BUS DIBC.MULTIMODAL
  case find (\config -> "corp_" `T.isPrefixOf` config.feedKey) configs of
    Nothing -> pure ShuttleInterestAPI.ShuttlePublishedRoutesResp {ShuttleInterestAPI.feedKey = Nothing, ShuttleInterestAPI.routes = []}
    Just config -> do
      baseUrl <- MM.getOTPRestServiceReq config.merchantId config.merchantOperatingCityId
      routes <- OTPRest.getRoutesByGtfsId config
      timetable <- QShuttlePublishedTrip.findAllByFeedKeyAndMerchantOperatingCityId config.feedKey merchantOperatingCity.id
      let day = mfilter (not . T.null) (T.toLower . T.strip <$> mbDay)
          rows = maybe timetable (\d -> filter (\t -> t.day == d) timetable) day
          byRoute = Map.fromListWith (<>) [(t.routeCode, [t]) | t <- rows]
          shown = maybe routes (const (filter (\r -> Map.member r.code byRoute) routes)) day
      published <- forM (sortOn (.code) shown) $ \route -> do
        stops <- OTPRest.getRouteStopMappingByRouteCode route.code config
        trip <- NandiFlow.getExampleTrip baseUrl config.feedKey route.code
        pure $ mkPublishedRoute route stops trip (Map.findWithDefault [] route.code byRoute)
      pure ShuttleInterestAPI.ShuttlePublishedRoutesResp {ShuttleInterestAPI.feedKey = Just config.feedKey, ShuttleInterestAPI.routes = published}

mkPublishedRoute :: DRoute.Route -> [DRouteStopMapping.RouteStopMapping] -> Maybe NandiTypes.TripDetails -> [DShuttlePublishedTrip.ShuttlePublishedTrip] -> ShuttleInterestAPI.ShuttlePublishedRoute
mkPublishedRoute route stops mbTrip timetable =
  ShuttleInterestAPI.ShuttlePublishedRoute
    { ShuttleInterestAPI.routeCode = route.code,
      ShuttleInterestAPI.shortName = route.shortName,
      ShuttleInterestAPI.name = route.longName,
      ShuttleInterestAPI.tripCount = route.dailyTripCount,
      ShuttleInterestAPI.firstDeparture = secondsToHHMM <$> start,
      ShuttleInterestAPI.stops = map mkStop (sortOn (.sequenceNum) stops),
      ShuttleInterestAPI.trips = mkTrips timetable
    }
  where
    tripStops = maybe [] (.stops) mbTrip
    start = (.scheduledDeparture) <$> listToMaybe (sortOn (.stopPosition) tripStops)
    minutesAt code = do
      stop <- find (\t -> t.stopCode == code) tripStops
      tripStart <- start
      pure ((stop.scheduledArrival - tripStart) `div` 60)
    mkStop stop =
      ShuttleInterestAPI.ShuttlePublishedStop
        { ShuttleInterestAPI.stopCode = stop.stopCode,
          ShuttleInterestAPI.name = stop.stopName,
          ShuttleInterestAPI.lat = fst (latLon stop.stopPoint),
          ShuttleInterestAPI.lon = snd (latLon stop.stopPoint),
          ShuttleInterestAPI.sequence = stop.sequenceNum,
          ShuttleInterestAPI.minutesFromStart = minutesAt stop.stopCode
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
  where
    dayIndex d = fromMaybe 7 (lookup d (zip ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"] [0 :: Int ..]))

latLon :: Maps.LatLong -> (Double, Double)
latLon point = (point.lat, point.lon)

-- | Seconds after midnight -> "HH:MM".
secondsToHHMM :: Int -> Text
secondsToHHMM secs = pad (secs `div` 3600 `mod` 24) <> ":" <> pad (secs `div` 60 `mod` 60)
  where
    pad n = T.justifyRight 2 '0' (show n)
