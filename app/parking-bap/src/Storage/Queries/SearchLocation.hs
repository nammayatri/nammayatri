{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SearchLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Types.Logging (HasLog)
import Domain.SearchLocation
import Storage.Tabular.SearchLocation

findById :: (EsqDBFlow m r, HasLog r) => Id SearchLocation -> m (Maybe SearchLocation)
findById parkingLocationId =
  runTransaction . findOne' $ do
    parkingLocation <- from $ table @SearchLocationT
    where_ $ parkingLocation ^. SearchLocationId ==. val (getId parkingLocationId)
    return parkingLocation

create :: SearchLocation -> SqlDB ()
create = create'