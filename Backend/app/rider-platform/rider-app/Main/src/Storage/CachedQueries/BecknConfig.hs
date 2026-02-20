{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.BecknConfig
  ( findAll,
    findByMerchantIdDomainAndVehicle,
    findByMerchantIdDomainandMerchantOperatingCityId,
    findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback,
  )
where

import BecknV2.OnDemand.Enums as Enums
import Domain.Types.BecknConfig as DBC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.BecknConfig as BeamM
import qualified Storage.Queries.BecknConfig as Queries

findAll :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [BecknConfig]
findAll = findAllWithKV [Se.Is BeamM.id $ Se.Not $ Se.Eq $ getId ""]

findByMerchantIdDomainAndVehicle :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Text -> Enums.VehicleCategory -> m (Maybe BecknConfig)
findByMerchantIdDomainAndVehicle merchantId domain vehicle = do
  Hedis.safeGet (makeMerchantIdDomainKey merchantId domain vehicle) >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchantIdDomainAndVehicle /=<< Queries.findByMerchantIdDomainAndVehicle (Just merchantId) domain vehicle

findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Id Merchant -> Text -> Enums.VehicleCategory -> m (Maybe BecknConfig)
findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCityId merchantId domain vehicle = do
  Hedis.safeGet (makeMerchantIdDomainVehicleAndMerchantOperatingCityIdKey merchantOperatingCityId merchantId domain vehicle) >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust (cacheMerchantIdDomainVehicleAndMerchantOperatingCityId merchantOperatingCityId merchantId domain vehicle) /=<< Queries.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback (Just merchantOperatingCityId) (Just merchantId) domain vehicle

cacheMerchantIdDomainAndVehicle :: (CacheFlow m r) => BecknConfig -> m ()
cacheMerchantIdDomainAndVehicle config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantId = fromMaybe (error "BecknConfig.merchantId is Nothing in cacheMerchantIdDomainAndVehicle") config.merchantId
  Hedis.setExp (makeMerchantIdDomainKey merchantId config.domain config.vehicleCategory) config expTime

cacheMerchantIdDomainVehicleAndMerchantOperatingCityId :: (CacheFlow m r) => Id MerchantOperatingCity -> Id Merchant -> Text -> Enums.VehicleCategory -> BecknConfig -> m ()
cacheMerchantIdDomainVehicleAndMerchantOperatingCityId merchantOperatingCityId merchantId domain vehicle config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantIdDomainVehicleAndMerchantOperatingCityIdKey merchantOperatingCityId merchantId domain vehicle) config expTime

makeMerchantIdDomainVehicleAndMerchantOperatingCityIdKey :: Id MerchantOperatingCity -> Id Merchant -> Text -> Enums.VehicleCategory -> Text
makeMerchantIdDomainVehicleAndMerchantOperatingCityIdKey merchantOperatingCityId merchantId domain vehicle = "CachedQueries:BecknConfig:MerchantOperatingCityId:" <> merchantOperatingCityId.getId <> ":MerchantId:" <> merchantId.getId <> ":Domain:" <> domain <> ":Vehicle:" <> show vehicle

makeMerchantIdDomainKey :: Id Merchant -> Text -> Enums.VehicleCategory -> Text
makeMerchantIdDomainKey merchantId domain vehicle = "CachedQueries:BecknConfig:MerchantId:" <> merchantId.getId <> ":Domain:" <> domain <> ":Vehicle:" <> show vehicle

findByMerchantIdDomainandMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Text -> Id MerchantOperatingCity -> m [BecknConfig]
findByMerchantIdDomainandMerchantOperatingCityId merchantId domain merchantOperatingCityId = do
  Hedis.safeGet (makeMerchantIdDomainandMerchantOperatingCityIdKey merchantId domain merchantOperatingCityId) >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = cacheMerchantIdDomainandMerchantOperatingCityId merchantId domain merchantOperatingCityId /=<< Queries.findByMerchantIdDomainandMerchantOperatingCityId (Just merchantId) domain (Just merchantOperatingCityId)

cacheMerchantIdDomainandMerchantOperatingCityId :: (CacheFlow m r) => Id Merchant -> Text -> Id MerchantOperatingCity -> [BecknConfig] -> m ()
cacheMerchantIdDomainandMerchantOperatingCityId merchantId domain merchantOperatingCityId config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantIdDomainandMerchantOperatingCityIdKey merchantId domain merchantOperatingCityId) config expTime

makeMerchantIdDomainandMerchantOperatingCityIdKey :: Id Merchant -> Text -> Id MerchantOperatingCity -> Text
makeMerchantIdDomainandMerchantOperatingCityIdKey merchantId domain merchantOperatingCityId = "CachedQueries:BecknConfig:MerchantId:" <> merchantId.getId <> ":Domain:" <> domain <> ":MerchantOperatingCityId:" <> merchantOperatingCityId.getId
