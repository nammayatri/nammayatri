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
  ( findByMerchantIdDomainAndVehicle,
  )
where

import BecknV2.OnDemand.Enums
import Domain.Types.BecknConfig
import Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BecknConfig as Queries

findByMerchantIdDomainAndVehicle :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Text -> VehicleCategory -> m (Maybe BecknConfig)
findByMerchantIdDomainAndVehicle merchantId domain vehicle = do
  Hedis.safeGet (makeMerchantIdDomainKey merchantId domain vehicle) >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchantIdDomainAndVehicle /=<< Queries.findByMerchantIdDomainAndVehicle (Just merchantId) domain vehicle

cacheMerchantIdDomainAndVehicle :: (CacheFlow m r) => BecknConfig -> m ()
cacheMerchantIdDomainAndVehicle config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  whenJust config.merchantId $ \mid ->
    Hedis.setExp (makeMerchantIdDomainKey mid config.domain config.vehicleCategory) config expTime

makeMerchantIdDomainKey :: Id Merchant -> Text -> VehicleCategory -> Text
makeMerchantIdDomainKey merchantId domain vehicle = "CachedQueries:BecknConfig:MerchantId:" <> merchantId.getId <> ":Domain:" <> domain <> ":Vehicle:" <> show vehicle
