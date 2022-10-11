{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Vehicle where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Person as DPers
import qualified Domain.Types.Vehicle as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import qualified Storage.Tabular.Organization as TOrg
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.Category"
derivePersistField "Variant.Variant"
derivePersistField "Domain.RegistrationCategory"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleT sql=vehicle
      driverId PersonTId
      organizationId TOrg.OrganizationTId
      variant Variant.Variant
      model Text
      color Text
      registrationNo Text
      capacity Int Maybe
      category Domain.Category Maybe
      make Text Maybe
      size Text Maybe
      energyType Text Maybe
      registrationCategory Domain.RegistrationCategory Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary driverId
      Unique VehicleRegistrationNo
      UniqueVehicleRegistrationNo registrationNo
      deriving Generic
    |]

instance TEntityKey VehicleT where
  type DomainKey VehicleT = Id DPers.Person
  fromKey (VehicleTKey _id) = fromKey _id
  toKey id = VehicleTKey $ toKey id

instance TType VehicleT Domain.Vehicle where
  fromTType VehicleT {..} = do
    return $
      Domain.Vehicle
        { driverId = fromKey driverId,
          organizationId = fromKey organizationId,
          ..
        }
  toTType Domain.Vehicle {..} =
    VehicleT
      { driverId = toKey driverId,
        organizationId = toKey organizationId,
        ..
      }
