module Storage.Queries.ParkingLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.ParkingLocation
import Storage.Tabular.ParkingLocation

findById :: Transactionable m => Id ParkingLocation -> m (Maybe ParkingLocation)
findById = Esq.findById

create :: ParkingLocation -> SqlDB ()
create = create'

findAll :: Transactionable m => m [ParkingLocation]
findAll =
  Esq.findAll $ do
    from $ table @ParkingLocationT
