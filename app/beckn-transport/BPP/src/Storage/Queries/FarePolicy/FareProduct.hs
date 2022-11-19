{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.FareProduct
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.FarePolicy.FareProduct
import Domain.Types.Merchant (Merchant)
import Storage.Tabular.FarePolicy.FareProduct

findEnabledByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m [FareProduct]
findEnabledByMerchantId = findEnabledByMerchantIdAndType Nothing

findEnabledByMerchantIdAndType ::
  Transactionable m =>
  Maybe FareProductType ->
  Id Merchant ->
  m [FareProduct]
findEnabledByMerchantIdAndType mbType merchantId =
  Esq.findAll $ do
    fareProduct <- from $ table @FareProductT
    where_ $
      fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
        &&. whenJust_ mbType (\typ -> fareProduct ^. FareProductProductType ==. val typ)
    pure fareProduct

insertIfNotExist ::
  Id Merchant ->
  FareProductType ->
  SqlDB ()
insertIfNotExist merchantId typ = do
  mbFp <- listToMaybe <$> findEnabledByMerchantIdAndType (Just typ) merchantId
  case mbFp of
    Nothing -> insertFareProduct
    Just _ -> pure ()
  where
    insertFareProduct :: SqlDB ()
    insertFareProduct = do
      now <- getCurrentTime
      guid <- Id <$> generateGUIDText
      Esq.create @_ @FareProduct $
        FareProduct
          { id = guid,
            merchantId = merchantId,
            _type = typ,
            createdAt = now
          }

delete ::
  Id Merchant ->
  FareProductType ->
  SqlDB ()
delete merchantId fpType = Esq.delete $ do
  fareProduct <- from $ table @FareProductT
  where_ $
    fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
      &&. fareProduct ^. FareProductProductType ==. val fpType
