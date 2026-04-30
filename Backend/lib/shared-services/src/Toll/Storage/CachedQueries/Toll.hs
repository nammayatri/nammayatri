{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Toll.Storage.CachedQueries.Toll where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (CacheFlow)
import Toll.Domain.Types.Toll
import Toll.Storage.BeamFlow (BeamFlow)
import qualified Toll.Storage.Queries.Toll as Queries

-- | merchantOpCityId is passed as Text so this module is consumable from any app
--   without importing the per-app MerchantOperatingCity domain type. Uses the shared
--   @BeamFlow@ constraint which carries the @HasSchemaName TollT@ requirement that
--   each app satisfies via the orphan instance in @Storage.Beam.Toll@.
findAllTollsByMerchantOperatingCity :: BeamFlow m r => Text -> m [Toll]
findAllTollsByMerchantOperatingCity merchantOpCityId =
  (Hedis.safeGet $ makeTollsKeyByMerchantOperatingCityId merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> cacheAllTollsByMerchantOperatingCity merchantOpCityId /=<< Queries.findAllTollsByMerchantOperatingCity (Just merchantOpCityId)

cacheAllTollsByMerchantOperatingCity :: (CacheFlow m r) => Text -> [Toll] -> m ()
cacheAllTollsByMerchantOperatingCity merchantOpCityId tolls = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeTollsKeyByMerchantOperatingCityId merchantOpCityId) tolls expTime

makeTollsKeyByMerchantOperatingCityId :: Text -> Text
makeTollsKeyByMerchantOperatingCityId merchantOpCityId = "CachedQueries:Toll:MerchantOpCityId-" <> merchantOpCityId
