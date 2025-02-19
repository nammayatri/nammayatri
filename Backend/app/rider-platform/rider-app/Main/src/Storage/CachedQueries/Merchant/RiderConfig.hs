{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.RiderConfig
  ( create,
    clearCache,
    findByMerchantOperatingCityId,
    findByMerchantOperatingCityIdInRideFlow,
  )
where

import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.RiderConfig
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.RiderConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => RiderConfig -> m ()
create = Queries.create

findByMerchantOperatingCityIdInRideFlow ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  [LYT.ConfigVersionMap] ->
  m (Maybe RiderConfig)
findByMerchantOperatingCityIdInRideFlow id configInExperimentVersions =
  findByMerchantOperatingCityId id (Just configInExperimentVersions)

findByMerchantOperatingCityId ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe [LYT.ConfigVersionMap] ->
  m (Maybe RiderConfig)
findByMerchantOperatingCityId id mbConfigInExperimentVersions = do
  DynamicLogic.findOneConfig (cast id) (LYT.RIDER_CONFIG LYT.RiderConfig) mbConfigInExperimentVersions Nothing (Queries.findByMerchantOperatingCityId id)

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCache id = DynamicLogic.clearConfigCache (cast id) (LYT.RIDER_CONFIG LYT.RiderConfig) Nothing
