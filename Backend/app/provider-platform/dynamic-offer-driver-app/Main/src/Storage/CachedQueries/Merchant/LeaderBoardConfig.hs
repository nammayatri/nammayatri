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
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, MonadFlow)
import Storage.Queries.LeaderBoardConfigs as Queries

create :: (MonadFlow m, CacheFlow m r, Esq.EsqDBFlow m r) => LeaderBoardConfigs -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> m [LeaderBoardConfigs]
findAllByMerchantOpCityId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return a
    Nothing -> cacheLeaderBoardConfigs id /=<< Queries.findAllByMerchantOpCityId id

cacheLeaderBoardConfigs :: CacheFlow m r => Id MerchantOperatingCity -> [LeaderBoardConfigs] -> m ()
cacheLeaderBoardConfigs merchantOperatingCityId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey merchantOperatingCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey cfg expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:LeaderBoardConfigs:MerchantOperatingCityId-" <> id.getId

findLeaderBoardConfigbyType :: (CacheFlow m r, Esq.EsqDBFlow m r) => LeaderBoardType -> Id MerchantOperatingCity -> m (Maybe LeaderBoardConfigs)
findLeaderBoardConfigbyType leaderBType merchantOpCityId =
  Hedis.safeGet (makeLeaderBoardConfigKey leaderBType merchantOpCityId) >>= \case
    Just config -> pure $ Just config
    Nothing -> flip whenJust (cacheLeaderBoardConfig leaderBType merchantOpCityId) /=<< Queries.findLeaderBoardConfigbyType leaderBType merchantOpCityId

makeLeaderBoardConfigKey :: LeaderBoardType -> Id MerchantOperatingCity -> Text
makeLeaderBoardConfigKey leaderBType merchantOpCityId = "LBCFG:" <> merchantOpCityId.getId <> ":" <> show leaderBType

cacheLeaderBoardConfig :: (CacheFlow m r) => LeaderBoardType -> Id MerchantOperatingCity -> LeaderBoardConfigs -> m ()
cacheLeaderBoardConfig leaderBType merchantOpCityId lbConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeLeaderBoardConfigKey leaderBType merchantOpCityId) lbConfig expTime

clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache merchantOperatingCityId = do
  Hedis.del (makeMerchantOpCityIdKey merchantOperatingCityId)
