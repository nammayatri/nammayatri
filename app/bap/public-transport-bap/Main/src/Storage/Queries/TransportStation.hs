{-# LANGUAGE TypeApplications #-}

module Storage.Queries.TransportStation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.TransportStation
import Storage.Tabular.TransportStation

findByStationCode :: Transactionable m => Text -> m (Maybe TransportStation)
findByStationCode stationCode =
  Esq.findOne $ do
    parkingLocation <- from $ table @TransportStationT
    where_ $ parkingLocation ^. TransportStationStationCode ==. val stationCode
    return parkingLocation

create :: TransportStation -> SqlDB ()
create = Esq.create

findAll :: Transactionable m => m [TransportStation]
findAll =
  Esq.findAll $ do
    from $ table @TransportStationT

findById :: Transactionable m => Id TransportStation -> m (Maybe TransportStation)
findById transportStationId =
  Esq.findOne $ do
    transportStation <- from $ table @TransportStationT
    where_ $ transportStation ^. TransportStationTId ==. val (TransportStationTKey $ getId transportStationId)
    return transportStation
