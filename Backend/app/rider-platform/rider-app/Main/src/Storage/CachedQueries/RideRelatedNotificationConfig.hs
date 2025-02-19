{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.RideRelatedNotificationConfig
  ( create,
    findAllByMerchantOperatingCityId,
    findAllByMerchantOperatingCityIdAndTimeDiffEvent,
    clearCache,
    findAllByMerchantOperatingCityIdInRideFlow,
    findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow,
  )
where

import Domain.Types.MerchantOperatingCity
import Domain.Types.RideRelatedNotificationConfig
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.RideRelatedNotificationConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => RideRelatedNotificationConfig -> m ()
create = Queries.create

findAllByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [RideRelatedNotificationConfig]
findAllByMerchantOperatingCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs (cast id) (LYT.RIDER_CONFIG LYT.RideRelatedNotificationConfig) mbConfigVersionMap Nothing (Queries.findAllByMerchantOperatingCityId id)

findAllByMerchantOperatingCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [RideRelatedNotificationConfig]
findAllByMerchantOperatingCityIdInRideFlow id configInExperimentVersions =
  findAllByMerchantOperatingCityId id (Just configInExperimentVersions)

findAllByMerchantOperatingCityIdAndTimeDiffEvent :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> TimeDiffEvent -> Maybe [LYT.ConfigVersionMap] -> m [RideRelatedNotificationConfig]
findAllByMerchantOperatingCityIdAndTimeDiffEvent id timeDiffEvent mbConfigVersionMap =
  DynamicLogic.findAllConfigsWithCacheKey (cast id) (LYT.RIDER_CONFIG LYT.RideRelatedNotificationConfig) mbConfigVersionMap Nothing (Queries.findAllByMerchantOperatingCityIdAndTimeDiffEvent id timeDiffEvent) (makeMerchantOpCityIdAndTimeDiffEventKey id timeDiffEvent)

findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> TimeDiffEvent -> [LYT.ConfigVersionMap] -> m [RideRelatedNotificationConfig]
findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow id timeDiffEvent configInExperimentVersions =
  findAllByMerchantOperatingCityIdAndTimeDiffEvent id timeDiffEvent (Just configInExperimentVersions)

makeMerchantOpCityIdAndTimeDiffEventKey :: Id MerchantOperatingCity -> TimeDiffEvent -> Text
makeMerchantOpCityIdAndTimeDiffEventKey id timeDiffEvent = "CachedQueries:RideRelatedNotificationConfig:MerchantOperatingCityId-" <> id.getId <> ":TimeDiffEvent-" <> show timeDiffEvent

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> TimeDiffEvent -> m ()
clearCache merchantOpCityId timeDiffEvent =
  DynamicLogic.clearConfigCacheWithPrefix (makeMerchantOpCityIdAndTimeDiffEventKey merchantOpCityId timeDiffEvent) (cast merchantOpCityId) (LYT.RIDER_CONFIG LYT.RideRelatedNotificationConfig) Nothing
