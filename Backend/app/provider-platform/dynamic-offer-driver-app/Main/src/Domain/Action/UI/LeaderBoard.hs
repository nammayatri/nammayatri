{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.LeaderBoard where

import Control.Monad
import Data.Aeson hiding (Success)
import qualified Data.HashMap.Strict as HM
import Data.Time hiding (getCurrentTime)
import Data.Time.Calendar ()
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Domain.Action.UI.Person
import Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import qualified Domain.Types.LeaderBoardConfigs as LConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person as SP
import Domain.Utils hiding (monthDiff)
import GHC.Num.Integer (integerToInt)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import qualified SharedLogic.Merchant as SMerchant
import Storage.CachedQueries.Merchant.LeaderBoardConfig as QLeaderConfig
import qualified Storage.Queries.Person as QPerson

data DriversInfo = DriversInfo
  { rank :: Integer,
    name :: Text,
    totalRides :: Int,
    totalDistance :: Meters,
    totalDistanceWithUnit :: Distance,
    isCurrentDriver :: Bool,
    gender :: Maybe SP.Gender
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data LeaderBoardRes = LeaderBoardRes
  { driverList :: [DriversInfo],
    lastUpdatedAt :: Maybe UTCTime,
    totalEligibleDrivers :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getDailyDriverLeaderBoard ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  m LeaderBoardRes
getDailyDriverLeaderBoard (personId, merchantId, merchantOpCityId) day = do
  now <- getCurrentTime
  distanceUnit <- SMerchant.getDistanceUnitByMerchantOpCity merchantOpCityId
  let currentDate = RideEndInt.getCurrentDate now
      dateDiff = diffDays currentDate day
  dailyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.DAILY merchantOpCityId Nothing >>= fromMaybeM (InternalError "Leaderboard configs not present")
  unless dailyLeaderBoardConfig.isEnabled . throwError $ InvalidRequest "Leaderboard Not Available"
  let numberOfSets = fromIntegral dailyLeaderBoardConfig.numberOfSets
  when (dateDiff > numberOfSets - 1 || dateDiff < 0) $
    throwError $ InvalidRequest "Date outside Range"
  getDriverListFromLeaderBoard (personId, merchantId, merchantOpCityId) day day (integerToInt dateDiff) dailyLeaderBoardConfig distanceUnit

getLastDayOfYear :: Integer -> Day
getLastDayOfYear year = fromGregorian year 12 31

getWeeklyDriverLeaderBoard ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) =>
  (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  Day ->
  m LeaderBoardRes
getWeeklyDriverLeaderBoard (personId, merchantId, merchantOpCityId) fromDate toDate = do
  now <- getCurrentTime
  distanceUnit <- SMerchant.getDistanceUnitByMerchantOpCity merchantOpCityId
  let currentDate = RideEndInt.getCurrentDate now
      (currWeekNumber, _) = sundayStartWeek currentDate
      (reqWeekNumber, reqDayIndex) = sundayStartWeek fromDate
      (lastWeekOfYear, _) = sundayStartWeek $ getLastDayOfYear $ getYearFromDay fromDate
  let weekDiff = (currWeekNumber - reqWeekNumber + lastWeekOfYear) `mod` lastWeekOfYear
  weeklyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.WEEKLY merchantOpCityId Nothing >>= fromMaybeM (InternalError "Leaderboard configs not present")
  unless weeklyLeaderBoardConfig.isEnabled . throwError $ InvalidRequest "Leaderboard Not Available"
  let numberOfSets = weeklyLeaderBoardConfig.numberOfSets
  when (weekDiff > numberOfSets - 1 || weekDiff < 0) $ throwError $ InvalidRequest "Week outside Range"
  when (diffDays toDate fromDate /= 6 || reqDayIndex /= 0) $ throwError $ InvalidRequest "Invalid Input"
  getDriverListFromLeaderBoard (personId, merchantId, merchantOpCityId) fromDate toDate weekDiff weeklyLeaderBoardConfig distanceUnit

getMonthlyDriverLeaderBoard :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) => (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Int -> m LeaderBoardRes
getMonthlyDriverLeaderBoard (personId, merchantId, merchantOpCityId) month = do
  now <- getCurrentTime
  distanceUnit <- SMerchant.getDistanceUnitByMerchantOpCity merchantOpCityId
  let currentDay = RideEndInt.getCurrentDate now
      fromDate = fromGregorian (getYearFromDay currentDay) month 1
      monthDiff = RideEndInt.getMonth currentDay - month
  monthlyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.MONTHLY merchantOpCityId Nothing >>= fromMaybeM (InternalError "Leaderboard configs not present")
  unless monthlyLeaderBoardConfig.isEnabled . throwError $ InvalidRequest "Leaderboard Not Available"
  when ((monthDiff < 0 && 12 + monthDiff > monthlyLeaderBoardConfig.numberOfSets - 1) || monthDiff > monthlyLeaderBoardConfig.numberOfSets - 1) $ throwError $ InvalidRequest "Month outside Range"
  getDriverListFromLeaderBoard (personId, merchantId, merchantOpCityId) fromDate fromDate monthDiff monthlyLeaderBoardConfig distanceUnit

getDriverListFromLeaderBoard :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) => (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Day -> Day -> Int -> LConfig.LeaderBoardConfigs -> DistanceUnit -> m LeaderBoardRes
getDriverListFromLeaderBoard (personId, _, merchantOpCityId) fromDate toDate dateDiff leaderBoardConfig distanceUnit = do
  now <- getCurrentTime
  let leaderBoardType = leaderBoardConfig.leaderBoardType
      driverLeaderBoardKey = RideEndInt.makeDriverLeaderBoardKey leaderBoardType False merchantOpCityId fromDate toDate
      cachedDriverLeaderBoardKey = RideEndInt.makeDriverLeaderBoardKey leaderBoardType True merchantOpCityId fromDate toDate
  driversWithScoresMap :: [(Text, Double)] <- concat <$> Redis.withNonCriticalRedis (Redis.get cachedDriverLeaderBoardKey)
  let driverIds = map (Id . fst) driversWithScoresMap
  driverDetailsMap <- HM.fromList . map (\driver -> (driver.id.getId, (fromMaybe "Driver" $ getPersonFullName driver, driver.gender))) <$> B.runInReplica (QPerson.getDriversByIdIn driverIds)
  (drivers', isCurrentDriverInTop) <-
    foldlM
      ( \(acc, isCurrentDriverInTop) ((driverId, score), index) -> do
          (fullName, gender) <- HM.lookup driverId driverDetailsMap & fromMaybeM (PersonFieldNotPresent "DriverDetails")
          let (totalRides, totalDistance) = RideEndInt.getRidesAndDistancefromZscore score leaderBoardConfig.zScoreBase
              isCurrentDriver = personId.getId == driverId
          pure
            ( acc
                <> [ DriversInfo
                       { rank = index,
                         name = fullName,
                         gender = Just gender,
                         totalDistanceWithUnit = convertMetersToDistance distanceUnit totalDistance,
                         ..
                       }
                   ],
              isCurrentDriverInTop || isCurrentDriver
            )
      )
      ([], False)
      (zip driversWithScoresMap [1, 2 ..])
  totalEligibleDrivers <- Redis.withNonCriticalRedis $ Redis.zCard driverLeaderBoardKey
  if not isCurrentDriverInTop && dateDiff == 0
    then do
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      mbCurrPersonRank <- Redis.withNonCriticalRedis $ Redis.zRevRank driverLeaderBoardKey personId.getId
      mbCurrDriverZscore <- Redis.withNonCriticalRedis $ Redis.zScore driverLeaderBoardKey personId.getId
      let currDriverZscore = fromMaybe 0 mbCurrDriverZscore
      currPersonRank <-
        case mbCurrPersonRank of
          Nothing -> Redis.withNonCriticalRedis $ Redis.zCard driverLeaderBoardKey
          Just rank -> pure rank
      let (currPersonTotalRides, currPersonTotalDistance) = RideEndInt.getRidesAndDistancefromZscore currDriverZscore leaderBoardConfig.zScoreBase
      currPersonFullName <- getPersonFullName person & fromMaybeM (PersonFieldNotPresent "firstName")
      let currDriverInfo = DriversInfo (currPersonRank + 1) currPersonFullName currPersonTotalRides currPersonTotalDistance (convertMetersToDistance distanceUnit currPersonTotalDistance) True (Just person.gender)
      return $ LeaderBoardRes (currDriverInfo : drivers') (Just now) (fromIntegral totalEligibleDrivers)
    else return $ LeaderBoardRes drivers' (Just now) (fromIntegral totalEligibleDrivers)

makeLeaderboardBackfillKeyLock :: Text
makeLeaderboardBackfillKeyLock = "Driver_leaderboard_backfill_key_lock"
