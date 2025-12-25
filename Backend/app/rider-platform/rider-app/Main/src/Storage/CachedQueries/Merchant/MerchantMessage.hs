{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantMessage
  ( create,
    findAllByMerchantOpCityId,
    findAllByMerchantOpCityIdInRideFlow,
    findByMerchantOperatingCityIdAndMessageKey,
    findByMerchantOperatingCityIdAndMessageKeyInRideFlow,
    clearCache,
    clearCacheById,
  )
where

import Domain.Types.MerchantMessage
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.MerchantMessage as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantMessage -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [MerchantMessage]
findAllByMerchantOpCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs (cast id) (LYT.RIDER_CONFIG LYT.MerchantMessage) mbConfigVersionMap Nothing (Queries.findAllByMerchantOpCityId id)

findByMerchantOperatingCityIdAndMessageKey :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> Maybe [LYT.ConfigVersionMap] -> m (Maybe MerchantMessage)
findByMerchantOperatingCityIdAndMessageKey id messageKey mbConfigVersionMap =
  DynamicLogic.findOneConfigWithCacheKey
    (cast id)
    (LYT.RIDER_CONFIG LYT.MerchantMessage)
    mbConfigVersionMap
    Nothing
    (Queries.findByMerchantOperatingCityIdAndMessageKey id messageKey)
    (makeMerchantOperatingCityIdAndMessageKey id messageKey)

findAllByMerchantOpCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [MerchantMessage]
findAllByMerchantOpCityIdInRideFlow id configVersionMap =
  findAllByMerchantOpCityId id (Just configVersionMap)

findByMerchantOperatingCityIdAndMessageKeyInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> [LYT.ConfigVersionMap] -> m (Maybe MerchantMessage)
findByMerchantOperatingCityIdAndMessageKeyInRideFlow id messageKey configVersionMap =
  findByMerchantOperatingCityIdAndMessageKey id messageKey (Just configVersionMap)

makeMerchantOperatingCityIdAndMessageKey :: Id MerchantOperatingCity -> MessageKey -> Text
makeMerchantOperatingCityIdAndMessageKey id messageKey = "CachedQueries:MerchantMessage:MerchantOperatingCityId-" <> id.getId <> ":MessageKey-" <> show messageKey

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> m ()
clearCache merchantOperatingCityId messageKey =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantOperatingCityIdAndMessageKey merchantOperatingCityId messageKey)
    (cast merchantOperatingCityId)
    (LYT.RIDER_CONFIG LYT.MerchantMessage)
    Nothing

clearCacheById :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCacheById merchantOperatingCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOperatingCityId)
    (LYT.RIDER_CONFIG LYT.MerchantMessage)
    Nothing
