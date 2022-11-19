module Storage.Queries.TransporterConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Merchant
import Domain.Types.TransporterConfig
import Storage.Tabular.TransporterConfig

findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe TransporterConfig)
findByMerchantId merchantId =
  Esq.findOne $ do
    config <- from $ table @TransporterConfigT
    where_ $
      config ^. TransporterConfigMerchantId ==. val (toKey merchantId)
    return config
