{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Common (Hours (..), Kilometers (..))
import Beckn.Types.Id
import qualified Domain.Types.RentalFarePolicy as Domain
import qualified Domain.Types.Vehicle as Vehicle
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalFarePolicyT sql=rental_fare_policy
      id Text
      organizationId OrganizationTId
      vehicleVariant Vehicle.Variant
      baseFare Amount
      baseDistance Int
      baseDuration Int
      extraKmFare Amount
      extraMinuteFare Amount
      driverAllowanceForDay Amount Maybe
      deleted Bool
      Primary id
      deriving Generic
    |]

instance TEntityKey RentalFarePolicyT where
  type DomainKey RentalFarePolicyT = Id Domain.RentalFarePolicy
  fromKey (RentalFarePolicyTKey _id) = Id _id
  toKey (Id id) = RentalFarePolicyTKey id

instance TEntity RentalFarePolicyT Domain.RentalFarePolicy where
  fromTEntity entity = do
    let RentalFarePolicyT {..} = entityVal entity
    return $
      Domain.RentalFarePolicy
        { id = Id id,
          organizationId = fromKey organizationId,
          baseDistance = Kilometers baseDistance,
          baseDuration = Hours baseDuration,
          ..
        }
  toTType Domain.RentalFarePolicy {..} =
    RentalFarePolicyT
      { id = getId id,
        organizationId = toKey organizationId,
        deleted = False,
        baseDistance = getKilometers baseDistance,
        baseDuration = getHours baseDuration,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
