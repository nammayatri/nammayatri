module Utils.Defaults where

import Data.Time
import Data.Time.Calendar (Day (..))
import Data.Time.Clock
import Data.Time.LocalTime
import EulerHS.Prelude

localTime :: LocalTime
localTime = LocalTime (ModifiedJulianDay 58930) (TimeOfDay 1 1 1)

orgId :: Text
orgId = "9617486e522f44c6b6d54f6ca0d2731d"

id :: Text
id = "762b65659b10460ca2ce93133b51385c"

countryCode :: Text
countryCode = "+91"
