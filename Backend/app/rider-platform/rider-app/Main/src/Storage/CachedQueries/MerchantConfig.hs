{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.MerchantConfig
  ( findAllByMerchantOperatingCityId,
    findAllByMerchantOperatingCityIdInRideFlow,
    clearCache,
  )
where

import Domain.Types.MerchantConfig
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.MerchantConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

findAllByMerchantOperatingCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [MerchantConfig]
findAllByMerchantOperatingCityIdInRideFlow id configInExperimentVersions =
  findAllByMerchantOperatingCityId id (Just configInExperimentVersions)

findAllByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [MerchantConfig]
findAllByMerchantOperatingCityId id mbConfigInExperimentVersions =
  DynamicLogic.findAllConfigs (cast id) (LYT.RIDER_CONFIG LYT.MerchantConfig) mbConfigInExperimentVersions Nothing (Queries.findAllByMerchantOperatingCityId id True)

clearCache :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> m ()
clearCache id = DynamicLogic.clearConfigCache (cast id) (LYT.RIDER_CONFIG LYT.MerchantConfig) Nothing
