{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.OneWayFarePolicy.PerExtraKmRate where

import qualified Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate as Domain
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney, Meters (..))
import Kernel.Types.Id
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

instance FromTType PerExtraKmRateT FullPerExtraKmRate where
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

instance ToTType PerExtraKmRateT FullPerExtraKmRate where
  toTType (merchantId, vehVar, Domain.PerExtraKmRate {..}) =
    PerExtraKmRateT
      { merchantId = toKey merchantId,
        vehicleVariant = vehVar,
        distanceRangeStart = distanceRangeStart,
        fare = fare,
        ..
      }
