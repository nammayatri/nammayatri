{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.PayoutSplitConfig
  ( create,
    createMany,
    findByAreaCityAndVariant,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PayoutSplitConfig as DPSC
import qualified Domain.Types.VehicleVariant as DVehicleVariant
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SpecialLocation
import qualified Storage.Queries.PayoutSplitConfig as Queries

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DPSC.PayoutSplitConfig -> m ()
create = Queries.create

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [DPSC.PayoutSplitConfig] -> m ()
createMany = Queries.createMany

findByAreaCityAndVariant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  SpecialLocation.Area ->
  Id DMOC.MerchantOperatingCity ->
  DVehicleVariant.VehicleVariant ->
  m (Maybe DPSC.PayoutSplitConfig)
findByAreaCityAndVariant area merchantOperatingCityId vehicleVariant = do
  Hedis.safeGet (makeAreaCityAndVariantKey area merchantOperatingCityId vehicleVariant) >>= \case
    Just a -> pure a
    Nothing ->
      ( \dataToBeCached -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp (makeAreaCityAndVariantKey area merchantOperatingCityId vehicleVariant) dataToBeCached expTime
      )
        /=<< fmap listToMaybe (Queries.findAllByAreasCityAndVariant [area] merchantOperatingCityId vehicleVariant)

makeAreaCityAndVariantKey ::
  SpecialLocation.Area ->
  Id DMOC.MerchantOperatingCity ->
  DVehicleVariant.VehicleVariant ->
  Text
makeAreaCityAndVariantKey area merchantOperatingCityId vehicleVariant =
  "driverOfferCachedQueries:PayoutSplitConfig:Area-"
    <> show area
    <> ":MerchantOperatingCityId-"
    <> Kernel.Types.Id.getId merchantOperatingCityId
    <> ":VehicleVariant-"
    <> show vehicleVariant
