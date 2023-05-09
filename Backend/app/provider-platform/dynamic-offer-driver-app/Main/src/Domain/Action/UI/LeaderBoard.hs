{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.LeaderBoard where

import Data.Aeson
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson

data DriversInfo = DriversInfo
  { rank :: Int,
    name :: Text,
    totalRides :: Int,
    totalDistance :: Meters
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype LeaderBoardRes = LeaderBoardRes
  { driverList :: [DriversInfo]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getDriverLeaderBoard :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) => (Id Person, Id DM.Merchant) -> Maybe Integer -> m LeaderBoardRes
getDriverLeaderBoard (_, merchantId) mbLimit = do
  config <- CQTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  driversSortedList <-
    Redis.get makeDriverLeaderBoardKey >>= \case
      Nothing -> do
        driversSortedList' <- Esq.runInReplica $ QDriverStats.getDriversSortedOrder mbLimit
        drivers' <- forM (zip [1, 2 ..] driversSortedList') $ \(index, driver) -> do
          person' <- Esq.runInReplica $ QPerson.findById (cast driver.driverId) >>= fromMaybeM (PersonDoesNotExist driver.driverId.getId)
          fullName <- getPersonFullName person' >>= fromMaybeM (PersonFieldNotPresent "firstName")
          pure DriversInfo {rank = index, name = fullName, totalRides = driver.totalRides, totalDistance = driver.totalDistance}
        Redis.setExp makeDriverLeaderBoardKey drivers' $ getSeconds (fromMaybe 3600 config.driverLeaderBoardExpiry)
        pure drivers'
      Just driversSortedList' -> return driversSortedList'
  return $ LeaderBoardRes driversSortedList
  where
    makeDriverLeaderBoardKey = "DriverLeaderBoard"
