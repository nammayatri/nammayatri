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
    findByPrimaryPhone,
    findAllByMerchantOpCityId,
    findAllCallExophoneByMerchantOpCityId,
    findAllExophones,
    updateAffectedPhones,
    deleteByMerchantOpCityId,
    clearCache,
    clearAllCache,
    findByEndRidePhone,
    findByMerchantOpCityIdServiceAndExophoneType,
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

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m [Exophone]
findAllByMerchantOpCityId merchantOpCityId =
  Hedis.safeGet (makeMerchantOpCityIdKey merchantOpCityId) >>= \case
    Just a -> return a
    Nothing -> cacheExophones merchantOpCityId /=<< Queries.findAllByMerchantOpCityId merchantOpCityId

findAllCallExophoneByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m [Exophone]
findAllCallExophoneByMerchantOpCityId merchantOpCityId = filter (\exophone -> exophone.exophoneType == CALL_RIDE) <$> findAllByMerchantOpCityId merchantOpCityId

findByPhone :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Exophone)
findByPhone phone = find (\exophone -> (exophone.primaryPhone == phone || exophone.backupPhone == phone) && exophone.exophoneType == CALL_RIDE) <$> findAllByPhone phone

findByEndRidePhone :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Exophone)
findByEndRidePhone phone = find (\exophone -> (exophone.primaryPhone == phone || exophone.backupPhone == phone) && exophone.exophoneType == END_RIDE) <$> findAllByPhone phone

findByPrimaryPhone :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Exophone)
findByPrimaryPhone phone = find (\exophone -> exophone.primaryPhone == phone && exophone.exophoneType == CALL_RIDE) <$> findAllByPhone phone

findAllByPhone :: (CacheFlow m r, EsqDBFlow m r) => Text -> m [Exophone]
findAllByPhone phone =
  Hedis.safeGet (makePhoneKey phone) >>= \case
    Nothing -> do
      exophones <- Queries.findAllByPhone phone
      case exophones of
        exophone : _ -> cacheExophones exophone.merchantOperatingCityId exophones
        _ -> pure ()
      pure exophones
    Just merchantOpCityId ->
      Hedis.safeGet (makeMerchantOpCityIdKey merchantOpCityId) >>= \case
        Just a -> return a
        Nothing -> cacheExophones merchantOpCityId /=<< Queries.findAllByPhone phone

findAllExophones :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [Exophone]
findAllExophones = Queries.findAllExophones

findByMerchantOpCityIdServiceAndExophoneType :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> CallService -> ExophoneType -> m [Exophone]
findByMerchantOpCityIdServiceAndExophoneType merchantOpCityId service exophoneType =
  Hedis.safeGet (makeMerchantOpCityIdServiceExophoneTypeKey merchantOpCityId service exophoneType) >>= \case
    Just a -> return a
    Nothing -> cacheExophonesByMerchantOpCityIdServiceAndExophoneType merchantOpCityId service exophoneType /=<< Queries.findByMerchantOpCityIdServiceAndExophoneType merchantOpCityId service exophoneType

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id DMOC.MerchantOperatingCity -> [Exophone] -> m ()
clearCache merchantOpCityId exophones = do
  Hedis.del (makeMerchantOpCityIdKey merchantOpCityId)
  forM_ exophones $ \exophone -> do
    Hedis.del (makePhoneKey exophone.primaryPhone)
    Hedis.del (makePhoneKey exophone.backupPhone)

clearAllCache :: Hedis.HedisFlow m r => m ()
clearAllCache = Hedis.delByPattern patternKey

-- test with empty list
cacheExophones :: CacheFlow m r => Id DMOC.MerchantOperatingCity -> [Exophone] -> m ()
cacheExophones merchantOpCityId exophones = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantOpCityIdKey = makeMerchantOpCityIdKey merchantOpCityId
  Hedis.setExp merchantOpCityIdKey exophones expTime
  forM_ exophones $ \exophone -> do
    Hedis.setExp (makePhoneKey exophone.primaryPhone) merchantOpCityId expTime
    Hedis.setExp (makePhoneKey exophone.backupPhone) merchantOpCityId expTime

cacheExophonesByMerchantOpCityIdServiceAndExophoneType :: CacheFlow m r => Id DMOC.MerchantOperatingCity -> CallService -> ExophoneType -> [Exophone] -> m ()
cacheExophonesByMerchantOpCityIdServiceAndExophoneType merchantOpCityId service exophoneType exophones = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantServiceAndExophoneTypeKey = makeMerchantOpCityIdServiceExophoneTypeKey merchantOpCityId service exophoneType
  Hedis.setExp merchantServiceAndExophoneTypeKey exophones expTime
  forM_ exophones $ \exophone -> do
    Hedis.setExp (makePhoneKey exophone.primaryPhone) merchantOpCityId expTime
    Hedis.setExp (makePhoneKey exophone.backupPhone) merchantOpCityId expTime

makeMerchantOpCityIdKey :: Id DMOC.MerchantOperatingCity -> Text
makeMerchantOpCityIdKey merchantOpCityId = "CachedQueries:Exophones:MerchantOpCityId-" <> merchantOpCityId.getId

makePhoneKey :: Text -> Text
makePhoneKey phone = "CachedQueries:Exophones:Phone-" <> phone

makeMerchantOpCityIdServiceExophoneTypeKey :: Id DMOC.MerchantOperatingCity -> CallService -> ExophoneType -> Text
makeMerchantOpCityIdServiceExophoneTypeKey merchantOpCityId service exophoneType = "CachedQueries:Exophones:MerchantOpCityId-" <> merchantOpCityId.getId <> ":CallService-" <> show service <> ":ExophoneType-" <> show exophoneType

patternKey :: Text
patternKey = "CachedQueries:Exophones:*"

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Exophone -> m ()
create = Queries.create

updateAffectedPhones :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Text] -> m ()
updateAffectedPhones = Queries.updateAffectedPhones

deleteByMerchantOpCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m ()
deleteByMerchantOpCityId = Queries.deleteByMerchantOpCityId
