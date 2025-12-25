{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Toll where

import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Toll
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.Toll as Queries

findAllTollsByMerchantOperatingCity :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> m [Toll]
findAllTollsByMerchantOperatingCity merchantOpCityId =
  (Hedis.safeGet $ makeTollsKeyByMerchantOperatingCityId merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> cacheAllTollsByMerchantOperatingCity merchantOpCityId /=<< Queries.findAllTollsByMerchantOperatingCity (Just merchantOpCityId)

cacheAllTollsByMerchantOperatingCity :: (CacheFlow m r) => Id MerchantOperatingCity -> [Toll] -> m ()
cacheAllTollsByMerchantOperatingCity merchantOpCityId tolls = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeTollsKeyByMerchantOperatingCityId merchantOpCityId) tolls expTime

makeTollsKeyByMerchantOperatingCityId :: Id MerchantOperatingCity -> Text
makeTollsKeyByMerchantOperatingCityId merchantOpCityId = "CachedQueries:Toll:MerchantOpCityId-" <> getId merchantOpCityId
