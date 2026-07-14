-- | Small pure helpers shared by the engine and message builders.
module WhatsappBot.Util
  ( fmtNum,
    fmtInt,
  )
where

import qualified Data.Text as T
import Kernel.Prelude

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
