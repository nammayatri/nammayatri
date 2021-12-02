{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Search where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Types.Logging (HasLog)
import Domain.Search
import Storage.Tabular.Search

findById :: (EsqDBFlow m r, HasLog r) => Id Search -> m (Maybe Search)
findById searchId =
  runTransaction . findOne' $ do
    search <- from $ table @SearchT
    where_ $ search ^. SearchId ==. val (getId searchId)
    return search

create :: Search -> SqlDB ()
create = create'
