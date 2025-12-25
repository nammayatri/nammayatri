{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.Merchant.LeaderBoardConfig where

import Domain.Types.LeaderBoardConfigs
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBFlow)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, MonadFlow)
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.LeaderBoardConfigs as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LeaderBoardConfigs -> m ()
create = Queries.create

findAllByMerchantOpCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [LeaderBoardConfigs]
findAllByMerchantOpCityIdInRideFlow id configVersionMap = findAllByMerchantOpCityId id (Just configVersionMap)

findLeaderBoardConfigbyTypeInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => LeaderBoardType -> Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m (Maybe LeaderBoardConfigs)
findLeaderBoardConfigbyTypeInRideFlow leaderBType merchantOpCityId configVersionMap = findLeaderBoardConfigbyType leaderBType merchantOpCityId (Just configVersionMap)

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [LeaderBoardConfigs]
findAllByMerchantOpCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs
    (cast id)
    (LYT.DRIVER_CONFIG LYT.LeaderBoardConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findAllByMerchantOpCityId id)

findLeaderBoardConfigbyType :: (CacheFlow m r, EsqDBFlow m r) => LeaderBoardType -> Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m (Maybe LeaderBoardConfigs)
findLeaderBoardConfigbyType leaderBType merchantOpCityId mbConfigVersionMap =
  DynamicLogic.findOneConfigWithCacheKey
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.LeaderBoardConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findLeaderBoardConfigbyType leaderBType merchantOpCityId)
    (makeLeaderBoardConfigKey leaderBType merchantOpCityId)

makeLeaderBoardConfigKey :: LeaderBoardType -> Id MerchantOperatingCity -> Text
makeLeaderBoardConfigKey leaderBType merchantOpCityId = "LBCFG:" <> merchantOpCityId.getId <> ":" <> show leaderBType

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCache merchantOpCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.LeaderBoardConfig)
    Nothing
