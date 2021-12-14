module Storage.Queries.ParkingLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.ParkingLocation
import Storage.Tabular.ParkingLocation

findById :: EsqDBFlow m r => Id ParkingLocation -> m (Maybe ParkingLocation)
findById = Esq.findById

create :: ParkingLocation -> SqlDB ()
create = create'

findAll :: EsqDBFlow m r => m [ParkingLocation]
findAll =
  runTransaction . findAll' $ do
    from $ table @ParkingLocationT
