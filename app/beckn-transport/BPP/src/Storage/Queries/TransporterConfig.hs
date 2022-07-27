module Storage.Queries.TransporterConfig where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.TransporterConfig
import Storage.Tabular.TransporterConfig

findValueByOrgIdAndKey :: Transactionable m => Id Organization -> ConfigKey -> m (Maybe TransporterConfig)
findValueByOrgIdAndKey orgId key_ =
  Esq.findOne $ do
    config <- from $ table @TransporterConfigT
    where_ $
      config ^. TransporterConfigTransporterId ==. val (toKey orgId)
        &&. config ^. TransporterConfigConfigKey ==. val key_
    return config
