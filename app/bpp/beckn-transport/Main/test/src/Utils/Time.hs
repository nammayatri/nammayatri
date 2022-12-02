module Utils.Time where

import qualified Data.Time as Time
import EulerHS.Prelude

parseTime :: String -> Time.UTCTime
parseTime time =
  let fmt = Time.iso8601DateFormat (Just "%H:%M:%S%QZ")
   in fromJust . Time.parseTimeM True Time.defaultTimeLocale fmt $ time
