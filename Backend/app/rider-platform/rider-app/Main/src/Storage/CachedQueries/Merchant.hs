{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant
  ( findById,
    findAll,
    findAllByExoPhone,
    findByShortId,
    findByExoPhone,
    update,
    clearCache,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant as Queries

findById :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe Merchant)
findById id =
  Hedis.safeGet (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
    Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id (Proxy @m)

findAll :: forall m r. (CacheFlow m r, EsqDBFlow m r) => m [Merchant]
findAll =
  Hedis.safeGet makeAllExoPhones >>= \case
    Just a -> return $ map (coerce @(MerchantD 'Unsafe) @Merchant) a
    Nothing -> cacheAllMerchants /=<< Queries.findAll (Proxy @m)

findByShortId :: forall m r. (CacheFlow m r, EsqDBFlow m r) => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId_ =
  Hedis.safeGet (makeShortIdKey shortId_) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByShortId shortId_ (Proxy @m)

findByExoPhone :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m (Maybe Merchant)
findByExoPhone countryCode exoPhone =
  Hedis.safeGet (makeExoPhoneKey countryCode exoPhone) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByExoPhone countryCode exoPhone (Proxy @m)

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Merchant -> m ()
clearCache merchant = do
  Hedis.del (makeIdKey merchant.id)
  Hedis.del (makeShortIdKey merchant.shortId)
  whenJust ((,) <$> merchant.exoPhoneCountryCode <*> merchant.exoPhone) $ \(exoPhoneCountryCode, exoPhone) ->
    Hedis.del (makeExoPhoneKey exoPhoneCountryCode exoPhone)

cacheMerchant :: (CacheFlow m r) => Merchant -> m ()
cacheMerchant merchant = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey merchant.id
  Hedis.setExp idKey (coerce @Merchant @(MerchantD 'Unsafe) merchant) expTime
  Hedis.setExp (makeShortIdKey merchant.shortId) idKey expTime
  whenJust ((,) <$> merchant.exoPhoneCountryCode <*> merchant.exoPhone) $ \(exoPhoneCountryCode, exoPhone) ->
    Hedis.setExp (makeExoPhoneKey exoPhoneCountryCode exoPhone) idKey expTime

cacheAllMerchants :: (CacheFlow m r) => [Merchant] -> m ()
cacheAllMerchants merchants = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp makeAllExoPhones (map (coerce @Merchant @(MerchantD 'Unsafe)) merchants) expTime

makeIdKey :: Id Merchant -> Text
makeIdKey id = "CachedQueries:Merchant:Id-" <> id.getId

makeShortIdKey :: ShortId Merchant -> Text
makeShortIdKey shortId = "CachedQueries:Merchant:ShortId-" <> shortId.getShortId

makeExoPhoneKey :: Text -> Text -> Text
makeExoPhoneKey countryCode phone = "CachedQueries:Merchant:ExoPhone-" <> countryCode <> phone

makeAllExoPhones :: Text
makeAllExoPhones = "CachedQueries:Merchant:All-Merchants"

update :: Hedis.HedisFlow m r => Merchant -> Esq.SqlDB m ()
update merchant = do
  Queries.update merchant
  Esq.finalize $ clearCache merchant

--TODO Rethink the implementation
findAllByExoPhone ::
  (CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m (Maybe Merchant)
findAllByExoPhone countryCode exoPhone = findAll <&> find (\merchant -> (elem exoPhone merchant.exoPhones) && Just countryCode == merchant.exoPhoneCountryCode)
