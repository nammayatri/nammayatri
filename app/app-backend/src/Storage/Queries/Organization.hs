module Storage.Queries.Organization where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Storage.Tabular.Organization

findOrgByShortId :: Transactionable m => ShortId Organization -> m (Maybe Organization)
findOrgByShortId shortId_ = do
  findOne $ do
    org <- from $ table @OrganizationT
    where_ $ org ^. OrganizationShortId ==. val (getShortId shortId_)
    return org