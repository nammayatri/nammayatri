module Storage.Queries.Organization
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Storage.Tabular.Organization

findByShortId :: Transactionable m => ShortId Organization -> m (Maybe Organization)
findByShortId shortId_ = do
  findOne $ do
    org <- from $ table @OrganizationT
    where_ $ org ^. OrganizationShortId ==. val (getShortId shortId_)
    return org
