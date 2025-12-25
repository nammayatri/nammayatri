{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.DriverPoolConfig
  {-# WARNING
    "This module contains direct calls to the table and redis. \
  \ But most likely you need a version from Cac with inMem results feature."
    #-}
  ( clearCache,
    create,
    findAllByMerchantOpCityId,
    findByMerchantOpCityIdAndTripDistance,
    findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh,
    update,
    findAllByMerchantOpCityIdInRideFlow,
  )
where

import qualified Domain.Types as DVST
import Domain.Types.DriverPoolConfig
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.DriverPoolConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverPoolConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> Maybe Value -> m [DriverPoolConfig]
findAllByMerchantOpCityId id mbConfigVersionMap extraDimensions =
  DynamicLogic.findAllConfigs (cast id) (LYT.DRIVER_CONFIG LYT.DriverPoolConfig) mbConfigVersionMap extraDimensions (Queries.findAllByMerchantOpCityId Nothing Nothing id)

findAllByMerchantOpCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> Maybe Value -> m [DriverPoolConfig]
findAllByMerchantOpCityIdInRideFlow id configInExperimentVersions extraDimensions =
  findAllByMerchantOpCityId id (Just configInExperimentVersions) extraDimensions

findByMerchantOpCityIdAndTripDistance :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe [LYT.ConfigVersionMap] -> Maybe Value -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistance merchantOpCityId tripDistance mbConfigVersionMap extraDimensions = find (\config -> config.tripDistance == tripDistance) <$> findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap extraDimensions

findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe DVST.ServiceTierType -> Text -> SL.Area -> Maybe [LYT.ConfigVersionMap] -> Maybe Value -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh merchantOpCityId tripDistance serviceTier tripCategory area mbConfigVersionMap extraDimensions = find (\config -> config.tripDistance == tripDistance && config.vehicleVariant == serviceTier && config.tripCategory == tripCategory && config.area == area) <$> findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap extraDimensions

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCache id = DynamicLogic.clearConfigCache (cast id) (LYT.DRIVER_CONFIG LYT.DriverPoolConfig) Nothing

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverPoolConfig -> m ()
update = Queries.updateByPrimaryKey
