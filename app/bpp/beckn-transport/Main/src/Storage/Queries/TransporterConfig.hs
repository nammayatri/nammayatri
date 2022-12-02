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

findValueByMerchantIdAndKey :: Transactionable m => Id Merchant -> ConfigKey -> m (Maybe TransporterConfig)
findValueByMerchantIdAndKey merchantId key =
  Esq.findOne $ do
    config <- from $ table @TransporterConfigT
    where_ $
      config ^. TransporterConfigTransporterId ==. val (toKey merchantId)
        &&. config ^. TransporterConfigConfigKey ==. val key
    return config
