-- | Small pure helpers shared by the engine and message builders.
module WhatsappBot.Util
  ( fmtNum,
    fmtInt,

    -- * A place for a shared pin
    placeOrPin,
    pinPlaceId,
    isPinPlace,
    emptyAddress,
  )
where

import qualified Data.Text as T
import Kernel.Prelude
import WhatsappBot.Types (BotAddress (..), BotPlace (..))

-- | Render a number the way JavaScript's @`${n}`@ template interpolation does,
-- so ported copy matches the TS output: an integer-valued double prints with no
-- decimal point (@40.0 -> "40"@), otherwise the shortest round-tripping decimal
-- (@2.5 -> "2.5"@, @3.2 -> "3.2"@). Our values (fares, per-km rates, 1dp km,
-- ratings) never reach scientific-notation territory.
fmtNum :: Double -> Text
fmtNum n
  | n == fromIntegral r = T.pack (show r)
  | otherwise = dropTrailingZeros (T.pack (show n))
  where
    r = round n :: Integer
    -- GHC's `show` already gives the shortest decimal; this only guards against
    -- an accidental trailing zero (belt-and-braces, no-op in practice).
    dropTrailingZeros t
      | T.isInfixOf "." t = let t' = T.dropWhileEnd (== '0') t in if T.isSuffixOf "." t' then T.dropEnd 1 t' else t'
      | otherwise = t

-- | Render an integral value as text (etaMin, elapsed seconds, counts).
fmtInt :: Int -> Text
fmtInt = T.pack . show

-- | The place for a shared pin: the reverse-geocode result, or, when that
-- lookup failed, a placeholder at the pin itself with no address and
-- 'pinPlaceId' as its id. Shared by the pickup pin, the drop pin and a pin
-- shared to save as a favourite.
placeOrPin :: Double -> Double -> Either e BotPlace -> BotPlace
placeOrPin lat lon = \case
  Right p -> p
  Left _ -> BotPlace {lat = lat, lon = lon, placeId = pinPlaceId lat lon, address = emptyAddress}

-- | The placeholder id of a place that is only a pin: @"lat,lon"@. Not a real
-- place id, so it is never stored as one (see 'isPinPlace').
pinPlaceId :: Double -> Double -> Text
pinPlaceId lat lon = fmtNum lat <> "," <> fmtNum lon

-- | Whether this place's id is the 'pinPlaceId' placeholder rather than a real
-- place id from a geocode or search.
isPinPlace :: BotPlace -> Bool
isPinPlace p = p.placeId == pinPlaceId p.lat p.lon

emptyAddress :: BotAddress
emptyAddress = BotAddress {area = Nothing, building = Nothing, city = Nothing, country = Nothing, state = Nothing, street = Nothing}
