module Beckn.Utils.Common where

import           Data.Time
import           Data.Time.Calendar  (Day (..))
import           Data.Time.Clock
import           Data.Time.LocalTime
import qualified EulerHS.Language    as L
import           EulerHS.Prelude

class GuidLike a where
  generateGUID :: L.Flow a
instance GuidLike Text where
  generateGUID = L.generateGUID

getCurrTime :: L.Flow LocalTime
getCurrTime = L.runIO $ do
  utc <- getCurrentTime
  timezone <- getTimeZone utc
  pure $ utcToLocalTime timezone utc

defaultLocalTime :: LocalTime
defaultLocalTime = LocalTime (ModifiedJulianDay 58870) (TimeOfDay 1 1 1)
