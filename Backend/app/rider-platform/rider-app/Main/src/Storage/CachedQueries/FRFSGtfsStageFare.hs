{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.FRFSGtfsStageFare
  ( findAllByVehicleTypeAndStageAndMerchantOperatingCityId,
  )
where

import qualified BecknV2.FRFS.Enums
import Domain.Types.FRFSGtfsStageFare
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSGtfsStageFare as Queries

findAllByVehicleTypeAndStageAndMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Prelude.Int -> Kernel.Types.Id.Id MerchantOperatingCity -> m [FRFSGtfsStageFare]
findAllByVehicleTypeAndStageAndMerchantOperatingCityId vehicleType stage merchantOperatingCityId = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.safeGet (gtfsStageFareCacheKey vehicleType stage merchantOperatingCityId) >>= \case
    Just fares -> return fares
    Nothing -> do
      fares <- Queries.findAllByVehicleTypeAndStageAndMerchantOperatingCityId vehicleType stage merchantOperatingCityId
      Hedis.setExp (gtfsStageFareCacheKey vehicleType stage merchantOperatingCityId) fares expTime
      return fares

gtfsStageFareCacheKey :: BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Prelude.Int -> Kernel.Types.Id.Id MerchantOperatingCity -> Text
gtfsStageFareCacheKey vehicleType stage merchantOperatingCityId = "CachedQueries:FRFSGtfsStageFare:VehicleType-" <> show vehicleType <> ":Stage-" <> show stage <> ":MerchantOperatingCityId-" <> getId merchantOperatingCityId
