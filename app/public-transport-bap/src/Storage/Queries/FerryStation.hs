module Storage.Queries.FerryStation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Domain.FerryStation
import Storage.Tabular.FerryStation

findByStationCode :: EsqDBFlow m r => Text -> m (Maybe FerryStation)
findByStationCode stationCode =
  runTransaction . findOne' $ do
    parkingLocation <- from $ table @FerryStationT
    where_ $ parkingLocation ^. FerryStationStationCode ==. val stationCode
    return parkingLocation

create :: FerryStation -> SqlDB ()
create = create'

findAll :: EsqDBFlow m r => m [FerryStation]
findAll =
  runTransaction . findAll' $ do
    from $ table @FerryStationT
