{-# LANGUAGE TypeApplications #-}

module Storage.Queries.TransportStation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Domain.Types.TransportStation
import Storage.Tabular.TransportStation

findByStationCode :: EsqDBFlow m r => Text -> m (Maybe TransportStation)
findByStationCode stationCode =
  runTransaction . findOne' $ do
    parkingLocation <- from $ table @TransportStationT
    where_ $ parkingLocation ^. TransportStationStationCode ==. val stationCode
    return parkingLocation

create :: TransportStation -> SqlDB ()
create = create'

findAll :: EsqDBFlow m r => m [TransportStation]
findAll =
  runTransaction . findAll' $ do
    from $ table @TransportStationT

findById :: EsqDBFlow m r => Id TransportStation -> m (Maybe TransportStation)
findById transportStationId =
  runTransaction . findOne' $ do
    transportStation <- from $ table @TransportStationT
    where_ $ transportStation ^. TransportStationTId ==. val (TransportStationTKey $ getId transportStationId)
    return transportStation
