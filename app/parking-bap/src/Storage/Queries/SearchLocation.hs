{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SearchLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.Logging (HasLog)
import qualified Storage.Domain.SearchLocation as Domain

findById :: (Esq.EsqDBFlow m r, HasLog r) => Id Domain.SearchLocation -> m (Maybe Domain.SearchLocation)
findById parkingSearchId =
  Esq.runTransaction . Esq.findOne' $ do
    parkingSearch <- Esq.from $ Esq.table @Domain.SearchLocationT
    Esq.where_ $ parkingSearch Esq.^. Domain.SearchLocationTId Esq.==. Esq.val (Domain.SearchLocationTKey $ getId parkingSearchId)
    return parkingSearch

create :: Domain.SearchLocation -> SqlDB Domain.SearchLocation
create = Esq.createReturningEntity'
