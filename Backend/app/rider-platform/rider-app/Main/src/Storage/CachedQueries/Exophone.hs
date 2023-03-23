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
    findAllByMerchantId,
    findRandomExophone,
    findAllExophones,
    updateAffectedPhones,
    deleteByMerchantId,
    clearCache,
    clearAllCache,
  )
where

import Data.List.NonEmpty (nonEmpty)
import Domain.Types.Exophone
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Randomizer
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Exophone as Queries

findAllByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m [Exophone]
findAllByMerchantId merchantId =
  Hedis.safeGet (makeMerchantIdKey merchantId) >>= \case
    Just a -> return a
    Nothing -> cacheExophones merchantId /=<< Queries.findAllByMerchantId merchantId

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m (Maybe Exophone)
findRandomExophone merchantId = do
  exophones <- findAllByMerchantId merchantId
  traverse getRandomElement $ nonEmpty exophones

findByPhone :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Exophone)
findByPhone phone = find (\exophone -> exophone.primaryPhone == phone || exophone.backupPhone == phone) <$> findAllByPhone phone

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

findAllExophones :: Esq.Transactionable m => m [Exophone]
findAllExophones = Queries.findAllExophones

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
    Hedis.setExp (makePhoneKey exophone.primaryPhone) merchantIdKey expTime
    Hedis.setExp (makePhoneKey exophone.backupPhone) merchantIdKey expTime

makeMerchantIdKey :: Id DM.Merchant -> Text
makeMerchantIdKey merchantId = "CachedQueries:Exophones:MerchantId-" <> merchantId.getId

makePhoneKey :: Text -> Text
makePhoneKey phone = "CachedQueries:Exophones:Phone-" <> phone

patternKey :: Text
patternKey = "CachedQueries:Exophones:*"

create :: Exophone -> Esq.SqlDB ()
create = Queries.create

updateAffectedPhones :: [Text] -> Esq.SqlDB ()
updateAffectedPhones = Queries.updateAffectedPhones

deleteByMerchantId :: Id DM.Merchant -> Esq.SqlDB ()
deleteByMerchantId = Queries.deleteByMerchantId
