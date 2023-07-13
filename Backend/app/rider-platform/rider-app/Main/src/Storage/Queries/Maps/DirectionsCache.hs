{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Maps.DirectionsCache
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Maps.DirectionsCache
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Maps.DirectionsCache

create :: DirectionsCache -> SqlDB ()
create = Esq.create

findRoute :: Transactionable m => Text -> Text -> Int -> m (Maybe DirectionsCache)
findRoute originHash destHash slot =
  Esq.findOne $ do
    directionsCache <- from $ table @DirectionsCacheT
    where_ $ directionsCache ^. DirectionsCacheOriginHash ==. val originHash &&. directionsCache ^. DirectionsCacheDestHash ==. val destHash &&. directionsCache ^. DirectionsCacheSlot ==. val slot
    return directionsCache
