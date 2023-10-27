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
import qualified Data.HashMap as HM
import Data.Time hiding (getCurrentTime)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.LeaderBoardConfig as LConfig
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import Storage.CachedQueries.Merchant.LeaderBoardConfig as QLeaderConfig
import qualified Storage.Queries.Person as QPerson

data DriversInfo = DriversInfo
  { rank :: Integer,
    name :: Text,
    totalRides :: Int,
    totalDistance :: Meters,
    isCurrentDriver :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data LeaderBoardRes = LeaderBoardRes
  { driverList :: [DriversInfo],
    lastUpdatedAt :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getDailyDriverLeaderBoard ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  m LeaderBoardRes
getDailyDriverLeaderBoard (personId, merchantId, merchantOpCityId) day = do
  now <- getCurrentTime
  let currentDate = RideEndInt.getCurrentDate now
  let dateDiff = diffDays currentDate day
  dailyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.DAILY merchantOpCityId >>= fromMaybeM (InternalError "Leaderboard configs not present")
  unless dailyLeaderBoardConfig.isEnabled . throwError $ InvalidRequest "Leaderboard Not Available"
  let numberOfSets = fromIntegral dailyLeaderBoardConfig.numberOfSets
  when (dateDiff > numberOfSets - 1 || dateDiff < 0) $
    throwError $ InvalidRequest "Date outside Range"
  driversWithScoresMap :: [(Text, Double)] <- concat <$> Redis.withNonCriticalRedis (Redis.get $ RideEndInt.makeCachedDailyDriverLeaderBoardKey merchantId day)
  let driverIds = map (Id . fst) driversWithScoresMap
  driverNamesMap :: HM.Map Text (Maybe Text) <- HM.fromList . map (\driver -> (driver.id.getId, getPersonFullName driver)) <$> QPerson.getDriversByIdIn driverIds
  (drivers', isCurrentDriverInTop) <-
    foldlM
      ( \(acc, isCurrentDriverInTop) ((driverId, score), index) -> do
          fullName <- join (HM.lookup driverId driverNamesMap) & fromMaybeM (PersonFieldNotPresent "firstName")
          let (totalRides, totalDistance) = RideEndInt.getRidesAndDistancefromZscore score dailyLeaderBoardConfig.zScoreBase
          let isCurrentDriver = personId.getId == driverId
          pure
            ( acc
                <> [ DriversInfo
                       { rank = index,
                         name = fullName,
                         ..
                       }
                   ],
              isCurrentDriverInTop || isCurrentDriver
            )
      )
      ([], False)
      (zip driversWithScoresMap [1, 2 ..])
  if not isCurrentDriverInTop && dateDiff == 0
    then do
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      mbCurrPersonRank <- Redis.withNonCriticalRedis $ Redis.zRevRank (RideEndInt.makeDailyDriverLeaderBoardKey merchantId day) personId.getId
      mbCurrDriverZscore <- Redis.withNonCriticalRedis $ Redis.zScore (RideEndInt.makeDailyDriverLeaderBoardKey merchantId day) personId.getId
      let currentDriverScore = fromMaybe 0 mbCurrDriverZscore
      currPersonRank <-
        case mbCurrPersonRank of
          Nothing -> Redis.withNonCriticalRedis $ Redis.zCard (RideEndInt.makeDailyDriverLeaderBoardKey merchantId day)
          Just rank -> pure rank
      let (currPersonTotalRides, currPersonTotalDistance) = RideEndInt.getRidesAndDistancefromZscore currentDriverScore dailyLeaderBoardConfig.zScoreBase
      currPersonFullName <- getPersonFullName person & fromMaybeM (PersonFieldNotPresent "firstName")
      let currDriverInfo = DriversInfo (currPersonRank + 1) currPersonFullName currPersonTotalRides currPersonTotalDistance True
      return $ LeaderBoardRes (currDriverInfo : drivers') (Just now)
    else return $ LeaderBoardRes drivers' (Just now)

getWeeklyDriverLeaderBoard ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) =>
  (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  Day ->
  m LeaderBoardRes
getWeeklyDriverLeaderBoard (personId, merchantId, merchantOpCityId) fromDate toDate = do
  now <- getCurrentTime
  let currentDate = RideEndInt.getCurrentDate now
  let (currWeekNumber, _) = sundayStartWeek currentDate
  let (reqWeekNumber, reqDayIndex) = sundayStartWeek fromDate
  let weekDiff = currWeekNumber - reqWeekNumber
  weeklyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.WEEKLY merchantOpCityId >>= fromMaybeM (InternalError "Leaderboard configs not present")
  unless weeklyLeaderBoardConfig.isEnabled . throwError $ InvalidRequest "Leaderboard Not Available"
  let numberOfSets = weeklyLeaderBoardConfig.numberOfSets
  when (weekDiff > numberOfSets - 1 || weekDiff < 0) $
    throwError $ InvalidRequest "Week outside Range"
  when (diffDays toDate fromDate /= 6 || reqDayIndex /= 0) $
    throwError $ InvalidRequest "Invalid Input"
  driversWithScoresMap :: [(Text, Double)] <- concat <$> Redis.withNonCriticalRedis (Redis.get $ RideEndInt.makeCachedWeeklyDriverLeaderBoardKey merchantId fromDate toDate)
  let driverIds = map (Id . fst) driversWithScoresMap
  driverNamesMap <- HM.fromList . map (\driver -> (driver.id.getId, getPersonFullName driver)) <$> QPerson.getDriversByIdIn driverIds
  (drivers', isCurrentDriverInTop) <-
    foldlM
      ( \(acc, isCurrentDriverInTop) ((driverId, score), index) -> do
          fullName <- join (HM.lookup driverId driverNamesMap) & fromMaybeM (PersonFieldNotPresent "firstName")
          let (totalRides, totalDistance) = RideEndInt.getRidesAndDistancefromZscore score weeklyLeaderBoardConfig.zScoreBase
          let isCurrentDriver = personId.getId == driverId
          pure
            ( acc
                <> [ DriversInfo
                       { rank = index,
                         name = fullName,
                         ..
                       }
                   ],
              isCurrentDriverInTop || isCurrentDriver
            )
      )
      ([], False)
      (zip driversWithScoresMap [1, 2 ..])
  if not isCurrentDriverInTop && weekDiff == 0
    then do
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      mbCurrPersonRank <- Redis.withNonCriticalRedis $ Redis.zRevRank (RideEndInt.makeWeeklyDriverLeaderBoardKey merchantId fromDate toDate) personId.getId
      mbCurrDriverZscore <- Redis.withNonCriticalRedis $ Redis.zScore (RideEndInt.makeWeeklyDriverLeaderBoardKey merchantId fromDate toDate) personId.getId
      let currDriverZscore = fromMaybe 0 mbCurrDriverZscore
      currPersonRank <-
        case mbCurrPersonRank of
          Nothing -> Redis.withNonCriticalRedis $ Redis.zCard (RideEndInt.makeWeeklyDriverLeaderBoardKey merchantId fromDate toDate)
          Just rank -> pure rank
      let (currPersonTotalRides, currPersonTotalDistance) = RideEndInt.getRidesAndDistancefromZscore currDriverZscore weeklyLeaderBoardConfig.zScoreBase
      currPersonFullName <- getPersonFullName person & fromMaybeM (PersonFieldNotPresent "firstName")
      let currDriverInfo = DriversInfo (currPersonRank + 1) currPersonFullName currPersonTotalRides currPersonTotalDistance True
      return $ LeaderBoardRes (currDriverInfo : drivers') (Just now)
    else return $ LeaderBoardRes drivers' (Just now)
