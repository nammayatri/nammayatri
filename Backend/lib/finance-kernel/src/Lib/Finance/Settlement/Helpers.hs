module Lib.Finance.Settlement.Helpers
  ( dayLevelDedupKey,
  )
where

import qualified Data.Text as T
import Data.Time (TimeZone (..), addDays, defaultTimeLocale, formatTime, localDay, utcToLocalTime)
import Kernel.Prelude

dayLevelDedupKey :: Text -> UTCTime -> Text
dayLevelDedupKey prefix now =
  let ist = TimeZone 330 False "IST"
      todayIst = localDay (utcToLocalTime ist now)
      yesterdayIst = addDays (-1) todayIst
      label = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" yesterdayIst
   in prefix <> "_" <> label
