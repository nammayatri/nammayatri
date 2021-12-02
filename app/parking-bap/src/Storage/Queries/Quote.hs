{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Types.Logging (HasLog)
import Domain.Quote
import Storage.Tabular.Quote

findById :: (EsqDBFlow m r, HasLog r) => Id Quote -> m (Maybe Quote)
findById quoteId =
  runTransaction . findOne' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteId ==. val (getId quoteId)
    return quote

create :: Quote -> SqlDB ()
create = create'
