module Storage.Queries.FerryStation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
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

findById :: EsqDBFlow m r => Id FerryStation -> m (Maybe FerryStation)
findById ferryStationId =
  runTransaction . findOne' $ do
    ferryStation <- from $ table @FerryStationT
    where_ $ ferryStation ^. FerryStationTId ==. val (FerryStationTKey $ getId ferryStationId)
    return ferryStation