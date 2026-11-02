{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.FRFSFareClass
  ( findAllEnabledByVehicleServiceTierId,
    clearCache,
  )
where

import Domain.Types.FRFSFareClass
import Domain.Types.FRFSVehicleServiceTier (FRFSVehicleServiceTier)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSFareClass as Queries

findAllEnabledByVehicleServiceTierId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id FRFSVehicleServiceTier -> m [FRFSFareClass]
findAllEnabledByVehicleServiceTierId vehicleServiceTierId = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.safeGet (fareClassCacheKey vehicleServiceTierId) >>= \case
    Just fareClasses -> return fareClasses
    Nothing -> do
      fareClasses <- Queries.findAllByVehicleServiceTierIdAndEnabled vehicleServiceTierId True
      Hedis.setExp (fareClassCacheKey vehicleServiceTierId) fareClasses expTime
      return fareClasses

clearCache :: CacheFlow m r => Id FRFSVehicleServiceTier -> m ()
clearCache vehicleServiceTierId =
  Hedis.runInMultiCloudRedisWrite $ Hedis.del (fareClassCacheKey vehicleServiceTierId)

fareClassCacheKey :: Id FRFSVehicleServiceTier -> Text
fareClassCacheKey vehicleServiceTierId = "CachedQueries:FRFSFareClass:Enabled:VehicleServiceTierId-" <> getId vehicleServiceTierId
