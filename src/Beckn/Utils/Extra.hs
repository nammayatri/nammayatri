module Beckn.Utils.Extra where

import           Data.Time
import qualified EulerHS.Language as L
import           EulerHS.Prelude

getCurrentTimeUTC :: L.Flow LocalTime
getCurrentTimeUTC = L.runIO' "getCurrentTimeUTC" getCurrentTimeUTC'

getCurrentTimeUTC' :: IO LocalTime
getCurrentTimeUTC' = (zonedTimeToLocalTime . utcToZonedTime utc ) <$> getCurrentTime
