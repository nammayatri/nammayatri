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

findById :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe Merchant)
findById id =
  Hedis.safeGet (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
    Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id

findAll :: (CacheFlow m r, EsqDBFlow m r) => m [Merchant]
findAll =
  Hedis.safeGet makeAllExoPhones >>= \case
    Just a -> return $ map (coerce @(MerchantD 'Unsafe) @Merchant) a
    Nothing -> cacheAllMerchants /=<< Queries.findAll

findByShortId :: (CacheFlow m r, EsqDBFlow m r) => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId_ =
  Hedis.safeGet (makeShortIdKey shortId_) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByShortId shortId_

findByExoPhone :: (CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m (Maybe Merchant)
findByExoPhone countryCode exoPhone =
  Hedis.safeGet (makeExoPhoneKey countryCode exoPhone) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByExoPhone countryCode exoPhone

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

update :: Merchant -> Esq.SqlDB ()
update = Queries.update

--TODO Rethink the implementation
findAllByExoPhone ::
  (CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m (Maybe Merchant)
findAllByExoPhone countryCode exoPhone = findAll <&> find (\merchant -> (elem exoPhone merchant.exoPhones) && Just countryCode == merchant.exoPhoneCountryCode)