module Storage.Queries.Organization where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization as Organization
import Storage.Tabular.Organization
import Utils.Common

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

updateOrganizationRec :: Organization -> SqlDB ()
updateOrganizationRec org = do
  now <- getCurrentTime
  update' $ \tbl -> do
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
