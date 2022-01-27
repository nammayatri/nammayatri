{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

<<<<<<< HEAD
module Storage.Tabular.TransportStation where
=======
<<<<<<< HEAD:app/parking-bap/src/Storage/Tabular/Search.hs
module Storage.Tabular.Search where
=======
module Storage.Tabular.TransportStation where
>>>>>>> Added confirm/on_confirm for public transport bap:app/public-transport-bap/src/Storage/Tabular/TransportStation.hs
>>>>>>> Added confirm/on_confirm for public transport bap

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Database.Persist.TH
<<<<<<< HEAD
import qualified Domain.Types.TransportStation as Domain
=======
<<<<<<< HEAD:app/parking-bap/src/Storage/Tabular/Search.hs
import qualified Domain.Search as Domain
=======
import qualified Domain.Types.TransportStation as Domain
>>>>>>> Added confirm/on_confirm for public transport bap:app/public-transport-bap/src/Storage/Tabular/TransportStation.hs
>>>>>>> Added confirm/on_confirm for public transport bap

mkPersist
  defaultSqlSettings
  [defaultQQ|
<<<<<<< HEAD
    TransportStationT sql=transport_station
      id Text
      name Text
      stationCode Text
      lat Double
      lon Double
      deriving Generic
      Primary id
    |]

=======
<<<<<<< HEAD:app/parking-bap/src/Storage/Tabular/Search.hs
    SearchT sql=search
=======
    TransportStationT sql=transport_station
>>>>>>> Added confirm/on_confirm for public transport bap:app/public-transport-bap/src/Storage/Tabular/TransportStation.hs
      id Text
      lat Double
      lon Double
      requestorId Text
      fromDate UTCTime
      toDate UTCTime
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

<<<<<<< HEAD:app/parking-bap/src/Storage/Tabular/Search.hs
instance TEntityKey SearchT where
  type DomainKey SearchT = Id Domain.Search
  fromKey (SearchTKey _id) = Id _id
  toKey id = SearchTKey id.getId

instance TEntity SearchT Domain.Search where
  fromTEntity entity = do
    let SearchT {..} = entityVal entity
    return $
      Domain.Search
=======
>>>>>>> Added confirm/on_confirm for public transport bap
instance TEntityKey TransportStationT where
  type DomainKey TransportStationT = Id Domain.TransportStation
  fromKey (TransportStationTKey _id) = Id _id
  toKey id = TransportStationTKey id.getId

instance TEntity TransportStationT Domain.TransportStation where
  fromTEntity entity = do
    let TransportStationT {..} = entityVal entity
    return $
      Domain.TransportStation
<<<<<<< HEAD
        { id = Id id,
          ..
        }

  toTType Domain.TransportStation {..} =
    TransportStationT
      { id = id.getId,
=======
>>>>>>> Added confirm/on_confirm for public transport bap:app/public-transport-bap/src/Storage/Tabular/TransportStation.hs
        { id = Id id,
          requestorId = Id requestorId,
          ..
        }
<<<<<<< HEAD:app/parking-bap/src/Storage/Tabular/Search.hs
  toTType Domain.Search {..} =
    SearchT
=======

  toTType Domain.TransportStation {..} =
    TransportStationT
>>>>>>> Added confirm/on_confirm for public transport bap:app/public-transport-bap/src/Storage/Tabular/TransportStation.hs
      { id = id.getId,
        requestorId = requestorId.getId,
>>>>>>> Added confirm/on_confirm for public transport bap
        ..
      }
  toTEntity a =
    Entity (toKey (Id a.stationCode)) $ toTType a
