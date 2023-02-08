module Storage.Queries.BlackListOrg
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.BlackListOrg
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.BlackListOrg

findByShortId :: Transactionable m => ShortId BlackListOrg -> m (Maybe BlackListOrg)
findByShortId shortId_ = do
  findOne $ do
    org <- from $ table @BlackListOrgT
    where_ $ org ^. BlackListOrgShortId ==. val (getShortId shortId_)
    return org
