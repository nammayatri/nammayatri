{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy
-- {-# WARNING
--   "This module contains direct calls to the table and redis. \
-- \ But most likely you need a version from Cac with inMem results feature."
--   #-}
  ( clearCache,
    create,
    delete,
    update',
    clearCacheById,
    findFarePolicyFromDB,
    findFarePolicyFromDB',
  )
where

import Data.Coerce (coerce)
import Data.Text as Text
import Domain.Types.Common
import Domain.Types.FarePolicy
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.FarePolicy as Queries
import qualified Tools.DynamicLogic as DynamicLogic

findFarePolicyFromDB :: (CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> m (Maybe FarePolicy)
findFarePolicyFromDB id = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
    Nothing -> do
      flip whenJust cacheFarePolicy /=<< Queries.findById id

findFarePolicyFromDB' :: (CacheFlow m r, EsqDBFlow m r, BeamFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> Maybe Value -> Id FarePolicy -> m (Maybe FarePolicy)
findFarePolicyFromDB' merchantOpCityId configVersionMap extraDimensions id = do
  DynamicLogic.findOneConfigWithCacheKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.FarePolicy) (Just configVersionMap) extraDimensions (Queries.findById id) (makeIdKey id)

cacheFarePolicy :: (CacheFlow m r) => FarePolicy -> m ()
cacheFarePolicy fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey fp.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @FarePolicy @(FarePolicyD 'Unsafe) fp) expTime

makeIdKey :: Id FarePolicy -> Text
makeIdKey id = "driver-offer:CachedQueries:FarePolicy:Id-" <> id.getId

-- Call it after any update
clearCache :: HedisFlow m r => FarePolicy -> m ()
clearCache fp = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fp.id)

clearCacheById :: HedisFlow m r => Id FarePolicy -> m ()
clearCacheById fid = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fid)

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FarePolicy -> m ()
create = Queries.create

delete :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> m ()
delete = Queries.delete

update' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FarePolicy -> m ()
update' = Queries.update'
