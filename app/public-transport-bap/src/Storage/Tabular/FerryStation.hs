{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FerryStation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Database.Persist.TH
import qualified Domain.FerryStation as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FerryStationT sql=ferry_station
      id Text
      name Text
      stationCode Text
      lat Double
      lon Double
      deriving Generic
      Primary id
    |]

instance TEntityKey FerryStationT Domain.FerryStation where
  fromKey (FerryStationTKey _id) = Id _id
  toKey id = FerryStationTKey id.getId

instance TEntity FerryStationT Domain.FerryStation where
  fromTEntity entity = do
    let FerryStationT {..} = entityVal entity
    return $
      Domain.FerryStation
        { id = Id id,
          ..
        }
  toTType Domain.FerryStation {..} =
    FerryStationT
      { id = id.getId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
