{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant
  ( findById,
    findByShortId,
    findByExoPhone,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant as Queries

findById :: (CacheFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> m (Maybe Merchant)
findById id =
  Hedis.get (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
    Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id

findByShortId :: (CacheFlow m r, EsqDBReplicaFlow m r) => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId_ =
  Hedis.get (makeShortIdKey shortId_) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.get (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByShortId shortId_

findByExoPhone :: (CacheFlow m r, EsqDBReplicaFlow m r) => Text -> Text -> m (Maybe Merchant)
findByExoPhone countryCode exoPhone =
  Hedis.get (makeExoPhoneKey countryCode exoPhone) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.get (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByExoPhone countryCode exoPhone

cacheMerchant :: (CacheFlow m r) => Merchant -> m ()
cacheMerchant merchant = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey merchant.id
  Hedis.setExp idKey (coerce @Merchant @(MerchantD 'Unsafe) merchant) expTime
  Hedis.setExp (makeShortIdKey merchant.shortId) idKey expTime
  whenJust ((,) <$> merchant.exoPhoneCountryCode <*> merchant.exoPhone) $ \(exoPhoneCountryCode, exoPhone) ->
    Hedis.setExp (makeExoPhoneKey exoPhoneCountryCode exoPhone) idKey expTime

makeIdKey :: Id Merchant -> Text
makeIdKey id = "CachedQueries:Merchant:Id-" <> id.getId

makeShortIdKey :: ShortId Merchant -> Text
makeShortIdKey shortId = "CachedQueries:Merchant:ShortId-" <> shortId.getShortId

makeExoPhoneKey :: Text -> Text -> Text
makeExoPhoneKey countryCode phone = "CachedQueries:Merchant:ExoPhone-" <> countryCode <> phone
