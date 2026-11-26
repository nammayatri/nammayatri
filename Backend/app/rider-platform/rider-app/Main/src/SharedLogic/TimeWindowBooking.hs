-- | Pure rules for TimeWindow ticket services (parking): duration tariff and booking-window checks.
-- Ported from ParkWise backend/src/lib/tariff.ts and booking.ts; keep results identical to those.
module SharedLogic.TimeWindowBooking
  ( DurationTariff (..),
    calculateFee,
    resolveBookingStart,
    snapBookingStart,
    checkBookingWindow,
    effectiveMaxMinutes,
    advanceBookingMaxDays,
    parkingHoldSeconds,
    overlaps,
    peakOverlap,
    localDay,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..), addDays, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..), Seconds (..))

-- | Stored in service_people_category.duration_tariff as {"tariffType": ..., "rules": ...};
-- "rules" is ParkWise's tariff_rules.rules_json unchanged.
data DurationTariff
  = FlatTariff Double
  | ProgressiveTariff Double [(Double, Double)] -- base fare, slabs of (min hours, rate per hour)
  | TimeBasedTariff [(Int, Int, Double)] -- local start minute, local end minute, rate per hour
  deriving (Show, Eq)

instance A.FromJSON DurationTariff where
  parseJSON = A.withObject "DurationTariff" $ \o -> do
    tariffType :: Text <- o A..: "tariffType"
    case tariffType of
      "flat" -> o A..: "rules" >>= A.withObject "flat" (\r -> FlatTariff <$> r A..:? "amount" A..!= 0)
      "progressive" ->
        o A..: "rules"
          >>= A.withObject "progressive" (\r -> ProgressiveTariff <$> r A..:? "base_fare" A..!= 0 <*> (r A..:? "slabs" A..!= [] >>= mapM slab))
      "time_based" -> o A..: "rules" >>= fmap TimeBasedTariff . mapM band
      other -> fail $ "unknown tariffType" <> T.unpack other
    where
      slab = A.withObject "slab" $ \s -> (,) <$> s A..: "min_hours" <*> s A..: "rate_per_hour"
      band = A.withObject "band" $ \b -> (,,) <$> (b A..: "start_time" >>= hhmm) <*> (b A..: "end_time" >>= hhmm) <*> b A..: "rate_per_hour"
      hhmm txt = case mapM (readMaybe . T.unpack) (T.splitOn ":" txt) of
        Just [h, m] -> pure (h * 60 + m)
        Just [h] -> pure (h * 60)
        _ -> fail $ "bad HH:MM " <> T.unpack txt

-- | Fee for [start, end), rounded half-up to 2 decimals like tariff.ts. Time-based bands use city-local clock time.
calculateFee :: Seconds -> DurationTariff -> UTCTime -> UTCTime -> HighPrecMoney
calculateFee tz tariff start end
  | end <= start = 0
  | otherwise = round2 $ case tariff of
    FlatTariff amount -> amount
    ProgressiveTariff base slabs -> progressiveFee base slabs (realToFrac (diffUTCTime end start) / 3600)
    TimeBasedTariff bands -> timeBasedFee bands
  where
    round2 :: Double -> HighPrecMoney
    round2 x = HighPrecMoney (toRational (floor (x * 100 + 0.5) :: Integer) / 100)

    timeBasedFee bands = go start 0
      where
        go cur acc
          | cur >= end = acc
          | otherwise =
            let next = min (addUTCTime 60 cur) end
             in go next (acc + realToFrac (diffUTCTime next cur) / 3600 * rateAt (localMinuteOfDay tz cur))
        rateAt m = maybe 0 (\(_, _, r) -> r) (find (covers m) bands)
        covers m (s, e, _)
          | s < e = m >= s && m < e
          | otherwise = m >= s || m < e -- overnight band, e.g. 22:00-06:00

progressiveFee :: Double -> [(Double, Double)] -> Double -> Double
progressiveFee base slabs hours = go base 0 (sortOn fst slabs)
  where
    go fee _ [] = fee
    go fee prev ((minHours, rate) : rest)
      | hours <= minHours = fee
      | otherwise =
        let next = maybe (1 / 0) fst (listToMaybe rest)
            inSlab = min hours next - max minHours prev
            fee' = if inSlab > 0 then fee + inSlab * rate else fee
         in if hours <= next then fee' else go fee' next rest

localMinuteOfDay :: Seconds -> UTCTime -> Int
localMinuteOfDay (Seconds tz) t = ((floor (utcTimeToPOSIXSeconds t) + tz) `div` 60) `mod` 1440

localDay :: Seconds -> UTCTime -> Day
localDay (Seconds tz) t = utctDay (addUTCTime (fromIntegral tz) t)

-- | A start within 120 seconds of now (or none) means now; quote and booking both use this rule.
snapBookingStart :: UTCTime -> Maybe UTCTime -> UTCTime
snapBookingStart now = maybe now (\start -> if abs (diffUTCTime start now) <= 120 then now else start)

-- | Omitted, or within 2 minutes of now, means start now. Otherwise the start must be in the future
-- and before local midnight ending today + advanceDays. Start-time granularity is left to the client.
resolveBookingStart :: Seconds -> Int -> UTCTime -> Maybe UTCTime -> Either Text UTCTime
resolveBookingStart tz@(Seconds tzSecs) advanceDays now = \case
  Nothing -> Right now
  Just start
    | abs (diffUTCTime start now) <= 120 -> Right now
    | start <= now -> Left "start time must be now or in the future"
    | start >= lastAllowed -> Left $ if advanceDays == 0 then "Advance bookings are allowed for today only" else "Advance bookings are allowed up to " <> show advanceDays <> " day(s) ahead"
    | otherwise -> Right start
  where
    lastAllowed = addUTCTime (fromIntegral (negate tzSecs)) (UTCTime (addDays (1 + fromIntegral advanceDays) (localDay tz now)) 0)

checkBookingWindow :: Int -> UTCTime -> UTCTime -> Either Text ()
checkBookingWindow maxMinutes start end
  | end <= start = Left "end time must be after start time"
  | diffUTCTime end start > fromIntegral (maxMinutes * 60) = Left $ "Parking duration cannot exceed " <> show maxMinutes <> " minutes for this zone"
  | otherwise = Right ()

-- | Zone max duration; unset or non-positive falls back to the 24h platform ceiling.
effectiveMaxMinutes :: Maybe Int -> Int
effectiveMaxMinutes = \case
  Just m | m > 0 -> m
  _ -> 1440

-- | rider_config.parkingConfig.advanceBookingMaxDays, default 0 (today only).
advanceBookingMaxDays :: Maybe A.Value -> Int
advanceBookingMaxDays mbCfg = max 0 . fromMaybe 0 $ mbCfg >>= A.parseMaybe (A.withObject "parkingConfig" (A..: "advanceBookingMaxDays"))

-- | rider_config.parkingConfig.holdSeconds: how long an unpaid booking holds its space, default 300.
parkingHoldSeconds :: Maybe A.Value -> NominalDiffTime
parkingHoldSeconds mbCfg = fromIntegral . max 0 . fromMaybe (300 :: Int) $ mbCfg >>= A.parseMaybe (A.withObject "parkingConfig" (A..: "holdSeconds"))

-- | An empty window (a stopped booking that never started) overlaps nothing.
overlaps :: UTCTime -> UTCTime -> (UTCTime, UTCTime) -> Bool
overlaps start end (s, e) = s < e && s < end && e > start

-- | Most windows active at once inside [start, end). Concurrency only rises at a window's start,
-- so checking at start and at each later window start is enough.
peakOverlap :: UTCTime -> UTCTime -> [(UTCTime, UTCTime)] -> Int
peakOverlap start end windows =
  let ws = filter (overlaps start end) windows
      points = start : [s | (s, _) <- ws, s > start]
   in maximum [length [() | (s, e) <- ws, s <= t, e > t] | t <- points]
