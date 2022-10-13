{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant
  ( findById,
    findByShortId,
    findByExoPhone,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant
import qualified Storage.Queries.Merchant as Queries

findById :: (HedisFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe Merchant)
findById id =
  Hedis.get (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
    Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id

findByShortId :: (HedisFlow m r, EsqDBFlow m r) => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId_ =
  Hedis.get (makeShortIdKey shortId_) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.get (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByShortId shortId_

findByExoPhone :: (HedisFlow m r, EsqDBFlow m r) => Text -> Text -> m (Maybe Merchant)
findByExoPhone countryCode exoPhone =
  Hedis.get (makeExoPhoneKey countryCode exoPhone) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.get (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByExoPhone countryCode exoPhone

cacheMerchant :: HedisFlow m r => Merchant -> m ()
cacheMerchant merchant = do
  let idKey = makeIdKey merchant.id
  Hedis.setExp idKey (coerce @Merchant @(MerchantD 'Unsafe) merchant) expTime
  Hedis.setExp (makeShortIdKey merchant.shortId) idKey expTime
  whenJust ((,) <$> merchant.exoPhoneCountryCode <*> merchant.exoPhone) $ \(exoPhoneCountryCode, exoPhone) ->
    Hedis.setExp (makeExoPhoneKey exoPhoneCountryCode exoPhone) idKey expTime
  where
    expTime = 60 * 60 * 24

makeIdKey :: Id Merchant -> Text
makeIdKey id = "CachedQueries:Merchant:Id:" <> id.getId

makeShortIdKey :: ShortId Merchant -> Text
makeShortIdKey shortId = "CachedQueries:Merchant:ShortId:" <> shortId.getShortId

makeExoPhoneKey :: Text -> Text -> Text
makeExoPhoneKey countryCode phone = "CachedQueries:Merchant:ExoPhone:" <> countryCode <> phone
