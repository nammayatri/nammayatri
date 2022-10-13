module Storage.Queries.TransporterConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.TransporterConfig
import Storage.Tabular.TransporterConfig

findValueByOrgIdAndKey :: Transactionable m => Id Organization -> ConfigKey -> m (Maybe TransporterConfig)
findValueByOrgIdAndKey orgId key =
  Esq.findOne $ do
    config <- from $ table @TransporterConfigT
    where_ $
      config ^. TransporterConfigTransporterId ==. val (toKey orgId)
        &&. config ^. TransporterConfigConfigKey ==. val key
    return config
