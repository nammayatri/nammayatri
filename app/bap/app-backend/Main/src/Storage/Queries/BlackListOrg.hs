module Storage.Queries.BlackListOrg
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.BlackListOrg
import Storage.Tabular.BlackListOrg

findByShortId :: Transactionable m => ShortId BlackListOrg -> m (Maybe BlackListOrg)
findByShortId shortId_ = do
  findOne $ do
    org <- from $ table @BlackListOrgT
    where_ $ org ^. BlackListOrgShortId ==. val (getShortId shortId_)
    return org
