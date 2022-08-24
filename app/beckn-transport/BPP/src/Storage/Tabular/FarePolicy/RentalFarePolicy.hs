{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (HighPrecMoney, Hours (..), Kilometers (..))
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy.RentalFarePolicy as Domain
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
      baseFare HighPrecMoney
      baseDistance Int
      baseDuration Int
      extraKmFare HighPrecMoney
      extraMinuteFare HighPrecMoney
      driverAllowanceForDay HighPrecMoney Maybe
      deleted Bool
      Primary id
      deriving Generic
    |]

instance TEntityKey RentalFarePolicyT where
  type DomainKey RentalFarePolicyT = Id Domain.RentalFarePolicy
  fromKey (RentalFarePolicyTKey _id) = Id _id
  toKey (Id id) = RentalFarePolicyTKey id

instance TType RentalFarePolicyT Domain.RentalFarePolicy where
  fromTType RentalFarePolicyT {..} = do
    let descriptions = Domain.mkDescriptions extraKmFare extraMinuteFare (roundToIntegral <$> driverAllowanceForDay)
    return $
      Domain.RentalFarePolicy
        { id = Id id,
          organizationId = fromKey organizationId,
          baseDistance = Kilometers baseDistance,
          baseDuration = Hours baseDuration,
          baseFare = roundToIntegral baseFare,
          driverAllowanceForDay = roundToIntegral <$> driverAllowanceForDay,
          ..
        }
  toTType Domain.RentalFarePolicy {..} =
    RentalFarePolicyT
      { id = getId id,
        organizationId = toKey organizationId,
        deleted = False,
        baseDistance = getKilometers baseDistance,
        baseDuration = getHours baseDuration,
        baseFare = fromIntegral baseFare,
        driverAllowanceForDay = fromIntegral <$> driverAllowanceForDay,
        ..
      }
