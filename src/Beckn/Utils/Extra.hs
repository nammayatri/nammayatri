module Beckn.Utils.Extra where

import EulerHS.Prelude
import qualified EulerHS.Language as L
import Data.Time

getCurrentTimeUTC :: L.Flow LocalTime
getCurrentTimeUTC = L.runIO' "getCurrentTimeUTC" getCurrentTimeUTC'

getCurrentTimeUTC' :: IO LocalTime
getCurrentTimeUTC' = (zonedTimeToLocalTime . utcToZonedTime utc ) <$> getCurrentTime
