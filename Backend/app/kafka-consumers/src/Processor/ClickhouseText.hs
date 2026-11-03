module Processor.ClickhouseText
  ( epochMsText,
  )
where

import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Kernel.Prelude

-- | Epoch ms as a DateTime64(3, 'UTC') value: "2026-10-27 06:30:00.123".
epochMsText :: Integer -> Text
epochMsText ms =
  T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (posixSecondsToUTCTime (fromInteger secs)))
    <> "."
    <> T.justifyRight 3 '0' (show millis)
  where
    (secs, millis) = ms `divMod` 1000
