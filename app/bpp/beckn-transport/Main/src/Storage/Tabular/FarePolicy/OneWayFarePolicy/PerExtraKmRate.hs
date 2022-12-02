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
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PerExtraKmRateT sql=one_way_fare_policy_per_extra_km_rate
      Id Int
      vehicleVariant Vehicle.Variant
      merchantId MerchantTId
      distanceRangeStart Meters
      fare HighPrecMoney
      deriving Generic
    |]

type FullPerExtraKmRate = (Id Merchant, Vehicle.Variant, Domain.PerExtraKmRate)

getDomainPart :: FullPerExtraKmRate -> Domain.PerExtraKmRate
getDomainPart (_, _, domain) = domain

instance TType PerExtraKmRateT FullPerExtraKmRate where
  fromTType PerExtraKmRateT {..} = do
    return
      ( fromKey merchantId,
        vehicleVariant,
        Domain.PerExtraKmRate
          { distanceRangeStart = distanceRangeStart,
            fare = fare,
            ..
          }
      )
  toTType (merchantId, vehVar, Domain.PerExtraKmRate {..}) =
    PerExtraKmRateT
      { merchantId = toKey merchantId,
        vehicleVariant = vehVar,
        distanceRangeStart = distanceRangeStart,
        fare = fare,
        ..
      }
