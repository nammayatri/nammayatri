{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.VehicleConfig
  ( findAllByBecknConfigId,
  )
where

import Domain.Types.BecknConfig
import Domain.Types.VehicleConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.VehicleConfig as Queries

findAllByBecknConfigId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id BecknConfig -> m [VehicleConfig]
findAllByBecknConfigId becknConfigId = do
  Hedis.safeGet (makeBecknConfigIdKey becknConfigId) >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = cacheBecknConfigId becknConfigId /=<< Queries.findAllByBecknConfigId becknConfigId

cacheBecknConfigId :: (CacheFlow m r) => Id BecknConfig -> [VehicleConfig] -> m ()
cacheBecknConfigId becknConfigId config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeBecknConfigIdKey becknConfigId) config expTime

makeBecknConfigIdKey :: Id BecknConfig -> Text
makeBecknConfigIdKey becknConfigId = "CachedQueries:VehicleConfig:BecknConfigId:" <> becknConfigId.getId
