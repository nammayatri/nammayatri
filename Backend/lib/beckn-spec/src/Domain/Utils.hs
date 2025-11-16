module Domain.Utils where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.Text as T
import Data.Time
import qualified Domain.Types.Trip as DTrip
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (length, map)
import Kernel.Types.Forkable
import Kernel.Types.Time

getVehicleAge :: Maybe Day -> UTCTime -> Maybe Months
getVehicleAge mfgDate now = fmap (\day -> Months $ monthDiff day (utctDay now)) mfgDate

-- TODO: Move below to SK Later
getYearFromDay :: Day -> Integer
getYearFromDay day = let (year, _, _) = toGregorian day in year

monthDiff :: Day -> Day -> Int
monthDiff day1 day2 =
  let (y1, m1, _) = toGregorian day1
      (y2, m2, _) = toGregorian day2
   in (fromIntegral y2 - fromIntegral y1) * 12 + (m2 - m1)

convertTextToUTC :: Maybe Text -> Maybe UTCTime
convertTextToUTC a = do
  a_ <- a
  parseTimeM True defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

utctTimeToDayOfWeek :: UTCTime -> DayOfWeek
utctTimeToDayOfWeek utcTime = dayOfWeek (utctDay utcTime)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : xs) = safeLast xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

mapConcurrently :: (L.MonadFlow m, Forkable m) => (a -> m b) -> [a] -> m [b]
mapConcurrently fn ar = do
  awaitables <- mapM (awaitableFork "mapConcurrently" . fn) ar
  results <- rights <$> mapM (L.await Nothing) awaitables
  return results

castTravelModeToVehicleCategory :: DTrip.MultimodalTravelMode -> Enums.VehicleCategory
castTravelModeToVehicleCategory DTrip.Bus = Enums.BUS
castTravelModeToVehicleCategory DTrip.Taxi = Enums.AUTO_RICKSHAW
castTravelModeToVehicleCategory DTrip.Walk = Enums.AUTO_RICKSHAW
castTravelModeToVehicleCategory DTrip.Metro = Enums.METRO
castTravelModeToVehicleCategory DTrip.Subway = Enums.SUBWAY
