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
    findAllByMerchantId,
    findAllCallExophoneByMerchantId,
    findAllExophones,
    updateAffectedPhones,
    deleteByMerchantId,
    clearCache,
    clearAllCache,
    findByEndRidePhone,
    findByMerchantServiceAndExophoneType,
  )
where

import Domain.Types.Exophone
import qualified Domain.Types.Merchant as DM
import Kernel.External.Call.Types (CallService)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Exophone as Queries

findAllByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m [Exophone]
findAllByMerchantId merchantId =
  Hedis.safeGet (makeMerchantIdKey merchantId) >>= \case
    Just a -> return a
    Nothing -> cacheExophones merchantId /=<< Queries.findAllByMerchantId merchantId

findAllCallExophoneByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m [Exophone]
findAllCallExophoneByMerchantId merchantId = filter (\exophone -> exophone.exophoneType == CALL_RIDE) <$> findAllByMerchantId merchantId

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
        exophone : _ -> cacheExophones exophone.merchantId exophones
        _ -> pure ()
      pure exophones
    Just merchantId ->
      Hedis.safeGet (makeMerchantIdKey merchantId) >>= \case
        Just a -> return a
        Nothing -> cacheExophones merchantId /=<< Queries.findAllByPhone phone

findAllExophones :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [Exophone]
findAllExophones = Queries.findAllExophones

findByMerchantServiceAndExophoneType :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> CallService -> ExophoneType -> m [Exophone]
findByMerchantServiceAndExophoneType merchantId service exophoneType =
  Hedis.safeGet (makeMerchantIdServiceExophoneTypeKey merchantId service exophoneType) >>= \case
    Just a -> return a
    Nothing -> cacheExophonesByMerchantServiceAndExophoneType merchantId service exophoneType /=<< Queries.findByMerchantServiceAndExophoneType merchantId service exophoneType

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id DM.Merchant -> [Exophone] -> m ()
clearCache merchantId exophones = do
  Hedis.del (makeMerchantIdKey merchantId)
  forM_ exophones $ \exophone -> do
    Hedis.del (makePhoneKey exophone.primaryPhone)
    Hedis.del (makePhoneKey exophone.backupPhone)

clearAllCache :: Hedis.HedisFlow m r => m ()
clearAllCache = Hedis.delByPattern patternKey

-- test with empty list
cacheExophones :: CacheFlow m r => Id DM.Merchant -> [Exophone] -> m ()
cacheExophones merchantId exophones = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantIdKey merchantId
  Hedis.setExp merchantIdKey exophones expTime
  forM_ exophones $ \exophone -> do
    Hedis.setExp (makePhoneKey exophone.primaryPhone) merchantId expTime
    Hedis.setExp (makePhoneKey exophone.backupPhone) merchantId expTime

cacheExophonesByMerchantServiceAndExophoneType :: CacheFlow m r => Id DM.Merchant -> CallService -> ExophoneType -> [Exophone] -> m ()
cacheExophonesByMerchantServiceAndExophoneType merchantId service exophoneType exophones = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantServiceAndExophoneTypeKey = makeMerchantIdServiceExophoneTypeKey merchantId service exophoneType
  Hedis.setExp merchantServiceAndExophoneTypeKey exophones expTime
  forM_ exophones $ \exophone -> do
    Hedis.setExp (makePhoneKey exophone.primaryPhone) merchantId expTime
    Hedis.setExp (makePhoneKey exophone.backupPhone) merchantId expTime

makeMerchantIdKey :: Id DM.Merchant -> Text
makeMerchantIdKey merchantId = "CachedQueries:Exophones:MerchantId-" <> merchantId.getId

makePhoneKey :: Text -> Text
makePhoneKey phone = "CachedQueries:Exophones:Phone-" <> phone

makeMerchantIdServiceExophoneTypeKey :: Id DM.Merchant -> CallService -> ExophoneType -> Text
makeMerchantIdServiceExophoneTypeKey merchantId service exophoneType = "CachedQueries:Exophones:MerchantId-" <> merchantId.getId <> ":CallService-" <> show service <> ":ExophoneType-" <> show exophoneType

patternKey :: Text
patternKey = "CachedQueries:Exophones:*"

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Exophone -> m ()
create = Queries.create

updateAffectedPhones :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Text] -> m ()
updateAffectedPhones = Queries.updateAffectedPhones

deleteByMerchantId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m ()
deleteByMerchantId = Queries.deleteByMerchantId
