{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.Logging (HasLog)
import qualified Storage.Domain.Quote as Domain

findById :: (Esq.EsqDBFlow m r, HasLog r) => Id Domain.Quote -> m (Maybe Domain.Quote)
findById parkingSearchId =
  Esq.runTransaction . Esq.findOne' $ do
    parkingSearch <- Esq.from $ Esq.table @Domain.QuoteT
    Esq.where_ $ parkingSearch Esq.^. Domain.QuoteTId Esq.==. Esq.val (Domain.QuoteTKey $ getId parkingSearchId)
    return parkingSearch

create :: Domain.Quote -> SqlDB Domain.Quote
create = Esq.createReturningEntity'
