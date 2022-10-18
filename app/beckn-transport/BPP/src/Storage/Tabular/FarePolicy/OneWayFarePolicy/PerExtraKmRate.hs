{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.OneWayFarePolicy.PerExtraKmRate where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (HighPrecMoney, Meters (..))
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate as Domain
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Vehicle as Vehicle
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PerExtraKmRateT sql=one_way_fare_policy_per_extra_km_rate
      Id Int
      vehicleVariant Vehicle.Variant
      organizationId OrganizationTId
      distanceRangeStart Meters
      fare HighPrecMoney
      deriving Generic
    |]

type FullPerExtraKmRate = (Id Organization, Vehicle.Variant, Domain.PerExtraKmRate)

getDomainPart :: FullPerExtraKmRate -> Domain.PerExtraKmRate
getDomainPart (_, _, domain) = domain

instance TType PerExtraKmRateT FullPerExtraKmRate where
  fromTType PerExtraKmRateT {..} = do
    return
      ( fromKey organizationId,
        vehicleVariant,
        Domain.PerExtraKmRate
          { distanceRangeStart = distanceRangeStart,
            fare = fare,
            ..
          }
      )
  toTType (orgId, vehVar, Domain.PerExtraKmRate {..}) =
    PerExtraKmRateT
      { organizationId = toKey orgId,
        vehicleVariant = vehVar,
        distanceRangeStart = distanceRangeStart,
        fare = fare,
        ..
      }
