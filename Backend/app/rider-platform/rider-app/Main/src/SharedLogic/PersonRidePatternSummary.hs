module SharedLogic.PersonRidePatternSummary
  ( defaultWindowDays,
    maxWindowDays,
    getOrComputeSummary,
    refreshOnRideEnd,
    refreshFreeFormSummaryIfStale,
    getLocalTimeOffset,
    SuggestionContext (..),
    ScoredRoute (..),
    rankRoutes,
    topService,
  )
where

import qualified API.Types.UI.PersonRidePattern as API
import qualified ChatCompletion.Interface.Types as CIT
import qualified ChatCompletion.Types as CT
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Geohash as Geohash
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, UTCTime (..), addDays, dayOfWeek, defaultTimeLocale, diffDays, formatTime, nominalDay)
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingStatus as DBS
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingStatus as DFTBS
import qualified Domain.Types.Location as DL
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonRidePatternSummary as DPRPS
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderConfig as DRC
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Randomizer (getRandomInRange)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.CachedQueries.PersonRidePatternSummary as CQPRPS
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonRidePatternSummary as QPRPS
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Transformers.Booking as TBooking
import qualified Tools.ChatCompletion as ChatCompletion

defaultWindowDays :: Int
defaultWindowDays = 30

maxWindowDays :: Int
maxWindowDays = 180

maxRecordsPerSource :: Int
maxRecordsPerSource = 1000

maxStoredRoutes :: Int
maxStoredRoutes = 25

routeGeohashPrecision :: Int
routeGeohashPrecision = 6

timeSlotMinutes :: Int
timeSlotMinutes = 30

maxTimeSlotsPerRoute :: Int
maxTimeSlotsPerRoute = 3

defaultTextRefreshDays :: Int
defaultTextRefreshDays = 7

summarizeLockSeconds :: Int
summarizeLockSeconds = 120

defaultTimeDiffFromUtcSeconds :: Int
defaultTimeDiffFromUtcSeconds = 19800

data Trip = Trip
  { routeKey :: Maybe Text,
    mode :: DPRPS.TravelMode,
    service :: Text,
    travelledAt :: UTCTime,
    fromGeohash :: Maybe Text,
    toGeohash :: Maybe Text,
    fromLatLong :: Maybe LatLong,
    toLatLong :: Maybe LatLong,
    fromAddress :: Maybe Text,
    toAddress :: Maybe Text,
    fromStopCode :: Maybe Text,
    toStopCode :: Maybe Text,
    fare :: Maybe HighPrecMoney
  }

getRiderConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m (Maybe DRC.RiderConfig)
getRiderConfig merchantOperatingCityId = getConfig (RiderConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing

getLocalTimeOffset :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m NominalDiffTime
getLocalTimeOffset merchantOperatingCityId = localTimeOffset <$> getRiderConfig merchantOperatingCityId

localTimeOffset :: Maybe DRC.RiderConfig -> NominalDiffTime
localTimeOffset = fromIntegral . maybe defaultTimeDiffFromUtcSeconds (.timeDiffFromUtc.getSeconds)

isAllowListed :: Maybe DRC.RiderConfig -> Id DP.Person -> Bool
isAllowListed mbRiderConfig personId = maybe False (elem personId.getId) (mbRiderConfig >>= (.ridePatternSummaryPersonIds))

tossRollout :: MonadFlow m => Maybe DRC.RiderConfig -> m Bool
tossRollout mbRiderConfig = case mbRiderConfig >>= (.ridePatternSummaryRolloutPercentage) of
  Just pct | pct > 0 -> (<= pct) <$> getRandomInRange (1, 100 :: Int)
  _ -> pure False

getOrComputeSummary :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m (Maybe DPRPS.PersonRidePatternSummary)
getOrComputeSummary personId =
  CQPRPS.findByPersonId personId >>= \case
    Just summary -> pure (Just summary)
    Nothing -> do
      person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      mbRiderConfig <- getRiderConfig person.merchantOperatingCityId
      if isAllowListed mbRiderConfig personId
        then do
          computeWithLock mbRiderConfig defaultWindowDays person
          CQPRPS.findByPersonId personId
        else pure Nothing

refreshOnRideEnd :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m ()
refreshOnRideEnd personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mbRiderConfig <- getRiderConfig person.merchantOperatingCityId
  QPRPS.findByPersonId personId >>= \case
    Just summary -> computeWithLock mbRiderConfig summary.analysisWindowDays person
    Nothing -> do
      eligible <- if isAllowListed mbRiderConfig personId then pure True else tossRollout mbRiderConfig
      when eligible $ computeWithLock mbRiderConfig defaultWindowDays person

computeWithLock :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe DRC.RiderConfig -> Int -> DP.Person -> m ()
computeWithLock mbRiderConfig windowDays person =
  Redis.whenWithLockRedis (computeLockKey person.id) computeLockSeconds $
    buildAndStoreSummary mbRiderConfig windowDays person

refreshFreeFormSummaryIfStale :: (ServiceFlow m r, CacheFlow m r, EsqDBFlow m r) => DPRPS.PersonRidePatternSummary -> m ()
refreshFreeFormSummaryIfStale summary = do
  now <- getCurrentTime
  mbRiderConfig <- getRiderConfig summary.merchantOperatingCityId
  let refreshAfter = fromIntegral (fromMaybe defaultTextRefreshDays (mbRiderConfig >>= (.ridePatternSummaryTextRefreshDays))) * nominalDay
      isStale = maybe True (\t -> diffUTCTime now t >= refreshAfter) summary.lastSummarizedAt
  when isStale $
    Redis.whenWithLockRedis (summarizeLockKey summary.personId) summarizeLockSeconds $ do
      let offset = localTimeOffset mbRiderConfig
          fallbackText = renderSummaryText offset summary
      text <- case mbRiderConfig >>= (.ridePatternSummaryLLMService) of
        Nothing -> pure fallbackText
        Just service ->
          withTryCatch "ridePatternSummary:llm" (summarizeWithLLM service offset summary) >>= \case
            Right (Just llmText) -> pure llmText
            Right Nothing -> pure fallbackText
            Left err -> do
              logError $ "LLM ride pattern summary failed for person " <> summary.personId.getId <> ": " <> show err
              pure fallbackText
      QPRPS.updateFreeFormSummary (Just text) (Just now) summary.personId
      CQPRPS.cacheSummary summary {DPRPS.freeFormSummary = Just text, DPRPS.lastSummarizedAt = Just now}

summarizeWithLLM :: (ServiceFlow m r, CacheFlow m r, EsqDBFlow m r) => CT.LLMChatCompletionService -> NominalDiffTime -> DPRPS.PersonRidePatternSummary -> m (Maybe Text)
summarizeWithLLM service offset summary = do
  let facts = renderSummaryText offset summary
      payload =
        TE.decodeUtf8 . BSL.toStrict . A.encode $
          A.object
            [ "windowDays" A..= summary.analysisWindowDays,
              "totalTrips" A..= summary.totalTrips,
              "completedTaxiRides" A..= summary.completedTaxiRides,
              "cancelledTaxiRides" A..= summary.cancelledTaxiRides,
              "scheduledTaxiRides" A..= summary.scheduledTaxiRides,
              "publicTransportTrips" A..= summary.publicTransportTrips,
              "serviceTakenCounts" A..= summary.serviceTakenCounts,
              "uniqueRoutesTravelled" A..= summary.uniqueRoutesTravelled,
              "topRoutes" A..= (take 5 . sortOn (\p -> Down p.count) $ M.elems summary.routePatterns),
              "tipComfortabilityPercent" A..= summary.tipComfortability,
              "medianTipValue" A..= summary.medianTipValue,
              "medianFareByService" A..= summary.medianFareByService,
              "hourOfDayCounts" A..= summary.hourOfDayCounts,
              "dayOfWeekCountsMonToSun" A..= summary.dayOfWeekCounts,
              "currency" A..= summary.currency
            ]
      req =
        CIT.GeneralChatCompletionReq
          { genMessages =
              [ CIT.GeneralChatCompletionMessage {genRole = "system", genContent = summarizationInstructions},
                CIT.GeneralChatCompletionMessage {genRole = "user", genContent = "Facts:\n" <> facts <> "\n\nStructured data (times are local):\n" <> payload}
              ]
          }
  resp <- ChatCompletion.getChatCompletion summary.merchantId summary.merchantOperatingCityId service req
  let text = T.strip resp.genMessage.genContent
  pure $ if T.null text then Nothing else Just text

summarizationInstructions :: Text
summarizationInstructions =
  "You write a short internal profile of a mobility app user's travel habits for personalisation. "
    <> "Using only the provided data, write 3 to 5 plain sentences covering how often they travel, which services they prefer, "
    <> "their regular routes and usual times, weekday vs weekend behaviour, and tipping habits. "
    <> "Do not invent facts, do not address the user, and do not use lists or markdown."

summarizeLockKey :: Id DP.Person -> Text
summarizeLockKey personId = "PersonRidePatternSummary:Summarize:PersonId-" <> personId.getId

computeLockKey :: Id DP.Person -> Text
computeLockKey personId = "PersonRidePatternSummary:Compute:PersonId-" <> personId.getId

computeLockSeconds :: Int
computeLockSeconds = 60

buildAndStoreSummary :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe DRC.RiderConfig -> Int -> DP.Person -> m ()
buildAndStoreSummary mbRiderConfig windowDays person = do
  let personId = person.id
      offset = localTimeOffset mbRiderConfig
  now <- getCurrentTime
  let windowStart = addUTCTime (negate $ fromIntegral windowDays * nominalDay) now
  bookings <- QBooking.findAllByRiderIdAndStatusesCreatedAfter personId [DBS.COMPLETED, DBS.CANCELLED] windowStart maxRecordsPerSource
  let completedBookings = filter ((== DBS.COMPLETED) . (.status)) bookings
      scheduledCount = length $ filter (.isScheduled) completedBookings
  rides <- QRide.findAllCompletedByBookingIds (map (.id) completedBookings)
  frfsBookings <- QFRFSTicketBooking.findAllByRiderIdAndStatusesCreatedAfter personId [DFTBS.CONFIRMED] windowStart maxRecordsPerSource
  let rideByBookingId = M.fromList [(ride.bookingId, ride) | ride <- rides]
      taxiTrips = [mkTaxiTrip booking (M.lookup booking.id rideByBookingId) | booking <- completedBookings]
      publicTransportTrips = map mkPublicTransportTrip frfsBookings
      trips = taxiTrips <> publicTransportTrips
      toLocal t = addUTCTime offset t
      today = utctDay (toLocal now)
      routeGroups = M.fromListWith (<>) [(key, [trip]) | trip <- trips, Just key <- [trip.routeKey]]
      allPatterns = M.mapMaybe (mkRoutePattern windowDays today toLocal) routeGroups
      storedPatterns = M.fromList . take maxStoredRoutes . sortOn (\(_, p) -> (Down p.count, Down p.lastTravelledAt)) $ M.toList allPatterns
      serviceOfBooking = M.fromList [(booking.id, show booking.vehicleServiceTierType) | booking <- completedBookings]
      tippedRides = [(svc, tip.amount) | ride <- rides, Just tip <- [ride.tipAmount], tip.amount > 0, Just svc <- [M.lookup ride.bookingId serviceOfBooking]]
      tipComfortability =
        if null rides
          then Nothing
          else Just $ roundToOneDecimal (100 * fromIntegral (length tippedRides) / fromIntegral (length rides))
      localTimes = map (toLocal . (.travelledAt)) trips
      currency =
        listToMaybe $
          mapMaybe (fmap (.currency) . (.totalFare)) rides
            <> map (.estimatedFare.currency) completedBookings
            <> map (.totalPrice.currency) frfsBookings
  mbExisting <- QPRPS.findByPersonId personId
  let summary =
        DPRPS.PersonRidePatternSummary
          { personId = personId,
            merchantId = person.merchantId,
            merchantOperatingCityId = person.merchantOperatingCityId,
            analysisWindowDays = windowDays,
            windowStart = windowStart,
            windowEnd = now,
            totalTrips = length trips,
            completedTaxiRides = length completedBookings,
            cancelledTaxiRides = length bookings - length completedBookings,
            scheduledTaxiRides = scheduledCount,
            publicTransportTrips = length publicTransportTrips,
            serviceTakenCounts = countBy (.service) trips,
            uniqueRoutesTravelled = M.size allPatterns,
            routePatterns = storedPatterns,
            tipComfortability = tipComfortability,
            medianTipValue = M.mapMaybe median $ M.fromListWith (<>) [(svc, [tip]) | (svc, tip) <- tippedRides],
            medianFareByService = M.mapMaybe median $ M.fromListWith (<>) [(trip.service, [f]) | trip <- trips, Just f <- [trip.fare]],
            hourOfDayCounts = histogram 24 (\t -> floor (utctDayTime t / 3600)) localTimes,
            dayOfWeekCounts = histogram 7 (weekdayIndex . utctDay) localTimes,
            lastTripAt = listToMaybe . sortOn Down $ map (.travelledAt) trips,
            currency = currency,
            freeFormSummary = mbExisting >>= (.freeFormSummary),
            lastSummarizedAt = mbExisting >>= (.lastSummarizedAt),
            createdAt = maybe now (.createdAt) mbExisting,
            updatedAt = now
          }
  if isJust mbExisting
    then QPRPS.updateByPrimaryKey summary
    else QPRPS.create summary
  CQPRPS.cacheSummary summary
  logInfo $ "Ride pattern summary built for person " <> personId.getId <> ": trips=" <> show summary.totalTrips <> ", routes=" <> show summary.uniqueRoutesTravelled

mkTaxiTrip :: DB.Booking -> Maybe DRide.Ride -> Trip
mkTaxiTrip booking mbRide =
  let fromLoc = booking.fromLocation
      mbToLoc = TBooking.getToLocation booking.bookingDetails
      fromGh = encodeGeohash fromLoc
      toGh = mbToLoc >>= encodeGeohash
   in Trip
        { routeKey = (\f t -> f <> "-" <> t) <$> fromGh <*> toGh,
          mode = DPRPS.TAXI,
          service = show booking.vehicleServiceTierType,
          travelledAt = fromMaybe booking.startTime (mbRide >>= (.rideStartTime)),
          fromGeohash = fromGh,
          toGeohash = toGh,
          fromLatLong = Just $ LatLong fromLoc.lat fromLoc.lon,
          toLatLong = (\l -> LatLong l.lat l.lon) <$> mbToLoc,
          fromAddress = displayAddress fromLoc,
          toAddress = mbToLoc >>= displayAddress,
          fromStopCode = Nothing,
          toStopCode = Nothing,
          fare = (.amount) <$> ((mbRide >>= (.totalFare)) <|> (mbRide >>= (.fare)) <|> Just booking.estimatedFare)
        }

mkPublicTransportTrip :: DFTB.FRFSTicketBooking -> Trip
mkPublicTransportTrip booking =
  let vehicle = show booking.vehicleType
   in Trip
        { routeKey = Just $ vehicle <> ":" <> booking.fromStationCode <> "-" <> booking.toStationCode,
          mode = DPRPS.PUBLIC_TRANSPORT,
          service = vehicle,
          travelledAt = fromMaybe booking.createdAt booking.startTime,
          fromGeohash = Nothing,
          toGeohash = Nothing,
          fromLatLong = Nothing,
          toLatLong = Nothing,
          fromAddress = Nothing,
          toAddress = Nothing,
          fromStopCode = Just booking.fromStationCode,
          toStopCode = Just booking.toStationCode,
          fare = Just booking.totalPrice.amount
        }

mkRoutePattern :: Int -> Day -> (UTCTime -> UTCTime) -> [Trip] -> Maybe DPRPS.RoutePattern
mkRoutePattern windowDays today toLocal trips =
  case sortOn (Down . (.travelledAt)) trips of
    [] -> Nothing
    latest : _ ->
      Just
        DPRPS.RoutePattern
          { count = length trips,
            frequency = frequencyBitmap windowDays today (map (utctDay . toLocal . (.travelledAt)) trips),
            timeOfDay = usualTimeSlots (map (minuteOfDay . toLocal . (.travelledAt)) trips),
            travelMode = latest.mode,
            fromGeohash = latest.fromGeohash,
            toGeohash = latest.toGeohash,
            fromLat = (.lat) <$> latest.fromLatLong,
            fromLon = (.lon) <$> latest.fromLatLong,
            toLat = (.lat) <$> latest.toLatLong,
            toLon = (.lon) <$> latest.toLatLong,
            fromAddress = latest.fromAddress,
            toAddress = latest.toAddress,
            fromStopCode = latest.fromStopCode,
            toStopCode = latest.toStopCode,
            serviceCounts = countBy (.service) trips,
            medianFare = median (mapMaybe (.fare) trips),
            lastTravelledAt = latest.travelledAt
          }

frequencyBitmap :: Int -> Day -> [Day] -> Text
frequencyBitmap windowDays today travelledDays =
  let firstDay = addDays (negate . fromIntegral $ windowDays - 1) today
      daySet = M.fromList [(d, ()) | d <- travelledDays]
   in T.pack [if M.member d daySet then '1' else '0' | d <- [firstDay .. today]]

usualTimeSlots :: [Int] -> [Text]
usualTimeSlots minutes =
  let slotOf m = (((m + timeSlotMinutes `div` 2) `div` timeSlotMinutes) * timeSlotMinutes) `mod` 1440
      ranked = sortOn (\(slot, c) -> (Down c, slot)) . M.toList $ M.fromListWith (+) [(slotOf m, 1 :: Int) | m <- minutes]
      recurring = filter ((>= 2) . snd) ranked
      chosen = take maxTimeSlotsPerRoute $ if null recurring then take 1 ranked else recurring
   in map (formatMinuteOfDay . fst) $ sortOn fst chosen

formatMinuteOfDay :: Int -> Text
formatMinuteOfDay m =
  let pad2 = T.justifyRight 2 '0' . show
   in pad2 (m `div` 60) <> ":" <> pad2 (m `mod` 60)

parseMinuteOfDay :: Text -> Maybe Int
parseMinuteOfDay t = case T.splitOn ":" t of
  [h, m] -> (\hh mm -> hh * 60 + mm) <$> readMaybe (T.unpack h) <*> readMaybe (T.unpack m)
  _ -> Nothing

minuteOfDay :: UTCTime -> Int
minuteOfDay t = floor (utctDayTime t / 60)

weekdayIndex :: Day -> Int
weekdayIndex d = (fromEnum (dayOfWeek d) - 1) `mod` 7

isWeekend :: Day -> Bool
isWeekend d = weekdayIndex d >= 5

histogram :: Int -> (a -> Int) -> [a] -> [Int]
histogram size bucketOf xs =
  let counts = M.fromListWith (+) [(bucketOf x, 1 :: Int) | x <- xs]
   in [M.findWithDefault 0 i counts | i <- [0 .. size - 1]]

countBy :: (a -> Text) -> [a] -> M.Map Text Int
countBy keyOf xs = M.fromListWith (+) [(keyOf x, 1) | x <- xs]

median :: [HighPrecMoney] -> Maybe HighPrecMoney
median [] = Nothing
median xs =
  let sorted = sort xs
      n = length sorted
      mid = n `div` 2
   in case drop (if even n then mid - 1 else mid) sorted of
        a : b : _ | even n -> Just ((a + b) / 2)
        a : _ -> Just a
        [] -> Nothing

roundToOneDecimal :: Double -> Double
roundToOneDecimal x = fromIntegral (round (x * 10) :: Int) / 10

encodeGeohash :: DL.Location -> Maybe Text
encodeGeohash loc = T.pack <$> Geohash.encode routeGeohashPrecision (loc.lat, loc.lon)

displayAddress :: DL.Location -> Maybe Text
displayAddress loc = loc.address.title <|> loc.address.area <|> loc.address.street

topService :: M.Map Text Int -> Maybe Text
topService = fmap fst . listToMaybe . sortOn (\(svc, c) -> (Down c, svc)) . M.toList

data SuggestionContext = SuggestionContext
  { localNow :: UTCTime,
    windowDays :: Int,
    windowEndLocalDay :: Day,
    origin :: Maybe LatLong
  }

data ScoredRoute = ScoredRoute
  { routeKey :: Text,
    routePattern :: DPRPS.RoutePattern,
    score :: Double,
    reasons :: [API.SuggestionReason]
  }

rankRoutes :: SuggestionContext -> M.Map Text DPRPS.RoutePattern -> [ScoredRoute]
rankRoutes ctx patterns =
  let maxCount = max 1 . maximum . (1 :) . map (.count) $ M.elems patterns
   in sortOn (Down . (.score)) . filter ((> minimumScore) . (.score)) . map (uncurry (scoreRoute ctx maxCount)) $ M.toList patterns

minimumScore :: Double
minimumScore = 0.05

scoreRoute :: SuggestionContext -> Int -> Text -> DPRPS.RoutePattern -> ScoredRoute
scoreRoute ctx maxCount key routePattern =
  let frequencyFactor = fromIntegral routePattern.count / fromIntegral maxCount
      nowMinute = minuteOfDay ctx.localNow
      slotDiffs = [circularMinuteDiff nowMinute m | Just m <- map parseMinuteOfDay routePattern.timeOfDay]
      closestSlot = if null slotDiffs then 720 else minimum slotDiffs
      timeFactor = max 0.1 (1 - fromIntegral closestSlot / 120)
      activeDays = [day | (day, '1') <- zip (bitmapDays ctx) (T.unpack routePattern.frequency)]
      lastActiveGap = case reverse activeDays of
        d : _ -> diffDays ctx.windowEndLocalDay d + diffDays (utctDay ctx.localNow) ctx.windowEndLocalDay
        [] -> fromIntegral ctx.windowDays
      recencyFactor = 1 / (1 + fromIntegral (max 0 lastActiveGap) / 7)
      todayIsWeekend = isWeekend (utctDay ctx.localNow)
      sameDayTypeShare =
        if null activeDays
          then 0.5
          else fromIntegral (length $ filter ((== todayIsWeekend) . isWeekend) activeDays) / fromIntegral (length activeDays)
      dayTypeFactor = 0.5 + 0.5 * sameDayTypeShare
      mbOriginDistance = do
        originPoint <- ctx.origin
        lat <- routePattern.fromLat
        lon <- routePattern.fromLon
        pure . highPrecMetersToMeters $ distanceBetweenInMeters originPoint (LatLong lat lon)
      originFactor = case mbOriginDistance of
        Nothing -> 1
        Just d
          | d <= 500 -> 1.5
          | d <= 1500 -> 1
          | otherwise -> 0.2
      reasons =
        [API.FREQUENT_ROUTE | frequencyFactor >= 0.5]
          <> [API.USUAL_TIME | closestSlot <= 45]
          <> [API.NEAR_ORIGIN | maybe False (<= 500) mbOriginDistance]
          <> [API.RECENTLY_TRAVELLED | lastActiveGap <= 7]
          <> [API.SAME_DAY_TYPE | sameDayTypeShare >= 0.7]
   in ScoredRoute
        { routeKey = key,
          routePattern = routePattern,
          score = roundToOneDecimal (100 * frequencyFactor * timeFactor * recencyFactor * dayTypeFactor * originFactor) / 100,
          reasons = reasons
        }

bitmapDays :: SuggestionContext -> [Day]
bitmapDays ctx = [addDays (negate . fromIntegral $ ctx.windowDays - 1) ctx.windowEndLocalDay .. ctx.windowEndLocalDay]

circularMinuteDiff :: Int -> Int -> Int
circularMinuteDiff a b = let d = abs (a - b) `mod` 1440 in min d (1440 - d)

renderSummaryText :: NominalDiffTime -> DPRPS.PersonRidePatternSummary -> Text
renderSummaryText offset summary
  | summary.totalTrips == 0 = "No completed trips in the last " <> show summary.analysisWindowDays <> " days."
  | otherwise =
    T.intercalate " " $
      catMaybes
        [ Just $
            "In the last " <> show summary.analysisWindowDays <> " days: " <> show summary.totalTrips <> " trips ("
              <> show summary.completedTaxiRides
              <> " taxi, "
              <> show summary.publicTransportTrips
              <> " public transport), "
              <> show summary.cancelledTaxiRides
              <> " cancelled taxi bookings"
              <> (if summary.scheduledTaxiRides > 0 then ", " <> show summary.scheduledTaxiRides <> " scheduled" else "")
              <> ".",
          nonEmpty' "Most used services: " (map (\(svc, c) -> svc <> " (" <> show c <> ")") . take 3 $ rankedCounts summary.serviceTakenCounts),
          nonEmpty' "Frequent routes: " (map describeRoute . take 3 . sortOn (\p -> (Down p.count, Down p.lastTravelledAt)) $ M.elems summary.routePatterns),
          (\pct -> "Tips on " <> showDecimal pct <> "% of rides" <> tipValues <> ".") <$> summary.tipComfortability,
          nonEmpty' "Most active hours: " (map (\h -> formatMinuteOfDay (h * 60) <> "-" <> formatMinuteOfDay (((h + 1) `mod` 24) * 60)) $ topBuckets summary.hourOfDayCounts),
          nonEmpty' "Most active days: " (map dayName $ topBuckets summary.dayOfWeekCounts),
          (\t -> "Last trip on " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (addUTCTime offset t)) <> ".") <$> summary.lastTripAt
        ]
  where
    nonEmpty' prefix items = if null items then Nothing else Just (prefix <> T.intercalate ", " items <> ".")
    rankedCounts = sortOn (\(svc, c) -> (Down c, svc)) . M.toList
    tipValues =
      if M.null summary.medianTipValue
        then ""
        else "; median tip " <> T.intercalate ", " [svc <> " " <> showDecimal (realToFrac v :: Double) | (svc, v) <- M.toList summary.medianTipValue]
    topBuckets counts = map fst . take 2 . filter ((> 0) . snd) . sortOn (\(i, c) -> (Down c, i)) $ zip [0 :: Int ..] counts
    describeRoute p =
      routeLabel p <> " " <> show p.count <> "x" <> maybe "" (" via " <>) (topService p.serviceCounts)
        <> (if null p.timeOfDay then "" else " around " <> T.intercalate "/" p.timeOfDay)
    routeLabel p = fromMaybe "?" (p.fromAddress <|> p.fromStopCode) <> " -> " <> fromMaybe "?" (p.toAddress <|> p.toStopCode)
    dayName i = fromMaybe "?" $ listToMaybe (drop i ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"])

showDecimal :: Double -> Text
showDecimal x =
  let r = roundToOneDecimal x
   in if r == fromIntegral (round r :: Int) then show (round r :: Int) else show r
