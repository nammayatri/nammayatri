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
import qualified Domain.Types.Vehicle as Domain
import qualified Storage.Tabular.Organization as TOrg

derivePersistField "Domain.Category"
derivePersistField "Domain.Variant"
derivePersistField "Domain.EnergyType"
derivePersistField "Domain.RegistrationCategory"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleT sql=vehicle
      id Text
      organizationId TOrg.OrganizationTId
      variant Domain.Variant
      model Text
      color Text
      registrationNo Text
      capacity Int Maybe
      category Domain.Category Maybe
      make Text Maybe
      size Text Maybe
      energyType Domain.EnergyType Maybe
      registrationCategory Domain.RegistrationCategory Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      Unique VehicleRegistrationNo
      deriving Generic
    |]

instance TEntityKey VehicleT where
  type DomainKey VehicleT = Id Domain.Vehicle
  fromKey (VehicleTKey _id) = Id _id
  toKey (Id id) = VehicleTKey id

instance TEntity VehicleT Domain.Vehicle where
  fromTEntity entity = do
    let VehicleT {..} = entityVal entity
    return $
      Domain.Vehicle
        { id = Id id,
          organizationId = fromKey organizationId,
          ..
        }
  toTType Domain.Vehicle {..} =
    VehicleT
      { id = getId id,
        organizationId = toKey organizationId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
