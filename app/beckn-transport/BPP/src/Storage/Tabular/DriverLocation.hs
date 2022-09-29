{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverLocation as Domain
import Domain.Types.Person (Person)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverLocationT sql=driver_location
      driverId PersonTId
      lat Double
      lon Double
      point Point
      coordinatesCalculatedAt UTCTime
      createdAt UTCTime
      updatedAt UTCTime
      Primary driverId
      UniqueDriverLocationId driverId
      deriving Generic
    |]

instance TEntityKey DriverLocationT where
  type DomainKey DriverLocationT = Id Person
  fromKey (DriverLocationTKey _id) = fromKey _id
  toKey id = DriverLocationTKey $ toKey id

instance TType DriverLocationT Domain.DriverLocation where
  fromTType DriverLocationT {..} = do
    return $
      Domain.DriverLocation
        { driverId = fromKey driverId,
          ..
        }
  toTType Domain.DriverLocation {..} =
    DriverLocationT
      { driverId = toKey driverId,
        point = Point,
        ..
      }
