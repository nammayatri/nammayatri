{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Exophone
  ( create,
    findByPhone,
    findAllByMerchantOperatingCityId,
    findAllExophones,
    updateAffectedPhones,
    deleteByMerchantOperatingCityId,
    clearCache,
    clearAllCache,
    findByPrimaryPhone,
    findByMerchantOperatingCityIdAndService,
  )
where

import Domain.Types.Exophone
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Kernel.External.Call.Types (CallService)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Exophone as Queries

findAllByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m [Exophone]
findAllByMerchantOperatingCityId merchantOperatingCityId =
  Hedis.safeGet (makeMerchantOperatingCityIdKey merchantOperatingCityId) >>= \case
    Just a -> return a
    Nothing -> cacheExophones merchantOperatingCityId /=<< Queries.findAllByMerchantOperatingCityId merchantOperatingCityId

findByPhone :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Exophone)
findByPhone phone = find (\exophone -> exophone.primaryPhone == phone || exophone.backupPhone == phone) <$> findAllByPhone phone

findByPrimaryPhone :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Exophone)
findByPrimaryPhone phone = find (\exophone -> exophone.primaryPhone == phone) <$> findAllByPhone phone

findAllByPhone :: (CacheFlow m r, EsqDBFlow m r) => Text -> m [Exophone]
findAllByPhone phone =
  Hedis.safeGet (makePhoneKey phone) >>= \case
    Nothing -> do
      exophones <- Queries.findAllByPhone phone
      case exophones of
        exophone : _ -> cacheExophones exophone.merchantOperatingCityId exophones
        _ -> pure ()
      pure exophones
    Just merchantOperatingCityId ->
      Hedis.safeGet (makeMerchantOperatingCityIdKey merchantOperatingCityId) >>= \case
        Just a -> return a
        Nothing -> cacheExophones merchantOperatingCityId /=<< Queries.findAllByPhone phone

findAllExophones :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [Exophone]
findAllExophones = Queries.findAllExophones

findByMerchantOperatingCityIdAndService :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> CallService -> m [Exophone]
findByMerchantOperatingCityIdAndService merchantOperatingCityId service =
  Hedis.safeGet (makeMerchantOperatingCityIdAndServiceKey merchantOperatingCityId service) >>= \case
    Just a -> return a
    Nothing -> cacheExophonesByMerchantOperatingCityIdAndService merchantOperatingCityId service /=<< Queries.findByMerchantOperatingCityIdAndService merchantOperatingCityId service

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id DMOC.MerchantOperatingCity -> [Exophone] -> m ()
clearCache merchantOperatingCityId exophones = do
  Hedis.del (makeMerchantOperatingCityIdKey merchantOperatingCityId)
  forM_ exophones $ \exophone -> do
    Hedis.del (makePhoneKey exophone.primaryPhone)
    Hedis.del (makePhoneKey exophone.backupPhone)

clearAllCache :: Hedis.HedisFlow m r => m ()
clearAllCache = Hedis.delByPattern patternKey

-- test with empty list
cacheExophones :: CacheFlow m r => Id DMOC.MerchantOperatingCity -> [Exophone] -> m ()
cacheExophones merchantOperatingCityId exophones = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantOperatingCityIdKey = makeMerchantOperatingCityIdKey merchantOperatingCityId
  Hedis.setExp merchantOperatingCityIdKey exophones expTime
  forM_ exophones $ \exophone -> do
    Hedis.setExp (makePhoneKey exophone.primaryPhone) merchantOperatingCityId expTime
    Hedis.setExp (makePhoneKey exophone.backupPhone) merchantOperatingCityId expTime

cacheExophonesByMerchantOperatingCityIdAndService :: CacheFlow m r => Id DMOC.MerchantOperatingCity -> CallService -> [Exophone] -> m ()
cacheExophonesByMerchantOperatingCityIdAndService merchantOperatingCityId service exophones = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantOperatingCityIdAndServiceKey = makeMerchantOperatingCityIdAndServiceKey merchantOperatingCityId service
  Hedis.setExp merchantOperatingCityIdAndServiceKey exophones expTime
  forM_ exophones $ \exophone -> do
    Hedis.setExp (makePhoneKey exophone.primaryPhone) merchantOperatingCityId expTime
    Hedis.setExp (makePhoneKey exophone.backupPhone) merchantOperatingCityId expTime

makeMerchantOperatingCityIdKey :: Id DMOC.MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey merchantOperatingCityId = "CachedQueries:Exophones:MerchantOperatingCityId-" <> merchantOperatingCityId.getId

makePhoneKey :: Text -> Text
makePhoneKey phone = "CachedQueries:Exophones:Phone-" <> phone

makeMerchantOperatingCityIdAndServiceKey :: Id DMOC.MerchantOperatingCity -> CallService -> Text
makeMerchantOperatingCityIdAndServiceKey merchantOperatingCityId service = "CachedQueries:Exophones:MerchantOperatingCityId-" <> merchantOperatingCityId.getId <> ":CallService-" <> show service

patternKey :: Text
patternKey = "CachedQueries:Exophones:*"

-- create :: Exophone -> Esq.SqlDB ()
create :: MonadFlow m => Exophone -> m ()
create = Queries.create

updateAffectedPhones :: MonadFlow m => [Text] -> m ()
updateAffectedPhones = Queries.updateAffectedPhones

deleteByMerchantOperatingCityId :: MonadFlow m => Id DMOC.MerchantOperatingCity -> m ()
deleteByMerchantOperatingCityId = Queries.deleteByMerchantOperatingCityId
