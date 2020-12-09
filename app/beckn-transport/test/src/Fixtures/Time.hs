module Fixtures.Time (defaultTime) where

import qualified Data.Time as Time
import EulerHS.Prelude

parseTime :: String -> Time.UTCTime
parseTime time =
  let fmt = Time.iso8601DateFormat (Just "%H:%M:%S%QZ")
   in fromJust . Time.parseTimeM True Time.defaultTimeLocale fmt $ time

defaultTime :: Time.UTCTime
defaultTime = parseTime "2019-11-28T07:30:54.471Z"
