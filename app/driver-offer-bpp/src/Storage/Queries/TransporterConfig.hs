module Storage.Queries.TransporterConfig where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.TransporterConfig
import Storage.Tabular.TransporterConfig

findValueByOrgId :: Transactionable m => Id Organization -> m (Maybe TransporterConfig)
findValueByOrgId orgId =
  Esq.findOne $ do
    config <- from $ table @TransporterConfigT
    where_ $
      config ^. TransporterConfigOrganizationId ==. val (toKey orgId)
    return config
