{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Search where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.Logging (HasLog)
import qualified Storage.Domain.Search as Domain

findById :: (Esq.EsqDBFlow m r, HasLog r) => Id Domain.Search -> m (Maybe Domain.Search)
findById parkingSearchId =
  Esq.runTransaction . Esq.findOne' $ do
    parkingSearch <- Esq.from $ Esq.table @Domain.SearchT
    Esq.where_ $ parkingSearch Esq.^. Domain.SearchTId Esq.==. Esq.val (Domain.SearchTKey $ getId parkingSearchId)
    return parkingSearch

create :: Domain.Search -> SqlDB Domain.Search
create = Esq.createReturningEntity'
