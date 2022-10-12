module Storage.Queries.Organization
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Organization as Organization
import Storage.Tabular.Organization

findById :: Transactionable m => Id Organization -> m (Maybe Organization)
findById = Esq.findById

loadAllProviders :: Transactionable m => m [Organization]
loadAllProviders =
  Esq.findAll $ do
    org <- from $ table @OrganizationT
    where_ $
      org ^. OrganizationStatus ==. val Organization.APPROVED
        &&. org ^. OrganizationDomain ==. val (Just Organization.MOBILITY)
        &&. org ^. OrganizationOrgType ==. val Organization.PROVIDER
        &&. org ^. OrganizationEnabled
    return org

update :: Organization -> SqlDB ()
update org = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ OrganizationName =. val org.name,
        OrganizationDescription =. val org.description,
        OrganizationHeadCount =. val org.headCount,
        OrganizationEnabled =. val org.enabled,
        OrganizationUpdatedAt =. val now,
        OrganizationFromTime =. val org.fromTime
      ]
    where_ $ tbl ^. OrganizationTId ==. val (toKey org.id)
