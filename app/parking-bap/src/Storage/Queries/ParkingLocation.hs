{-# LANGUAGE TypeApplications #-}

module Storage.Queries.ParkingLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Domain.ParkingLocation
import Storage.Tabular.ParkingLocation

findById :: EsqDBFlow m r => Id ParkingLocation -> m (Maybe ParkingLocation)
findById parkingLocationId =
  runTransaction . findOne' $ do
    parkingLocation <- from $ table @ParkingLocationT
    where_ $ parkingLocation ^. ParkingLocationTId ==. val (ParkingLocationTKey $ getId parkingLocationId)
    return parkingLocation

create :: ParkingLocation -> SqlDB ()
create = create'
