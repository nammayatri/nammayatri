{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.FRFSConfig
  ( findByMerchantOperatingCityId,
    findByMerchantOperatingCityIdInRideFlow,
    clearCache,
  )
where

import Domain.Types.FRFSConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.FRFSConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

findByMerchantOperatingCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m (Maybe Domain.Types.FRFSConfig.FRFSConfig)
findByMerchantOperatingCityIdInRideFlow merchantOperatingCityId configInExperimentVersions = do
  findByMerchantOperatingCityId merchantOperatingCityId (Just configInExperimentVersions)

findByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r) => Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m (Maybe Domain.Types.FRFSConfig.FRFSConfig)
findByMerchantOperatingCityId merchantOperatingCityId mbConfigInExperimentVersions = do
  DynamicLogic.findOneConfig (cast merchantOperatingCityId) (LYT.RIDER_CONFIG LYT.FRFSConfig) mbConfigInExperimentVersions Nothing (Queries.findByMerchantOperatingCityId merchantOperatingCityId)

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ()
clearCache merchantOperatingCityId = DynamicLogic.clearConfigCache (cast merchantOperatingCityId) (LYT.RIDER_CONFIG LYT.FRFSConfig) Nothing
