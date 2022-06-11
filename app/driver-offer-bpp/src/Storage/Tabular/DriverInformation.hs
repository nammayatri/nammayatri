{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverInformation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverInformation as Domain
import Domain.Types.Person (Person)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverInformationT sql=driver_information
      driverId PersonTId
      active Bool
      onRide Bool
      enabled Bool
      createdAt UTCTime
      updatedAt UTCTime
      Primary driverId
      deriving Generic
    |]

instance TEntityKey DriverInformationT where
  type DomainKey DriverInformationT = Id Person
  fromKey (DriverInformationTKey _id) = fromKey _id
  toKey id = DriverInformationTKey $ toKey id

instance TEntity DriverInformationT Domain.DriverInformation where
  fromTEntity entity = do
    let DriverInformationT {..} = entityVal entity
    return $
      Domain.DriverInformation
        { driverId = fromKey driverId,
          ..
        }
  toTType Domain.DriverInformation {..} =
    DriverInformationT
      { driverId = toKey driverId,
        ..
      }
  toTEntity a =
    Entity (toKey a.driverId) $ toTType a
