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

module Storage.Tabular.FarePolicy where

import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Centesimal, HighPrecMoney, Meters, Money, Seconds)
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FarePolicyT sql=fare_policy
      id Text
      merchantId MerchantTId
      vehicleVariant Variant.Variant

      baseDistanceFare HighPrecMoney
      baseDistanceMeters Meters
      perExtraKmFare HighPrecMoney
      deadKmFare Money
      driverMinExtraFee Money
      driverMaxExtraFee Money

      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe
      nightShiftRate Centesimal Maybe

      maxAllowedTripDistance Meters Maybe
      minAllowedTripDistance Meters Maybe

      waitingChargePerMin Money Maybe
      waitingTimeEstimatedThreshold Seconds Maybe

      createdAt UTCTime
      updatedAt UTCTime
      UniqueFarePolicyId id
      Primary id
      deriving Generic
    |]

instance TEntityKey FarePolicyT where
  type DomainKey FarePolicyT = Id Domain.FarePolicy
  fromKey (FarePolicyTKey _id) = Id _id
  toKey (Id id) = FarePolicyTKey id

instance TType FarePolicyT Domain.FarePolicy where
  fromTType FarePolicyT {..} = do
    let driverExtraFee =
          Domain.ExtraFee
            { minFee = driverMinExtraFee,
              maxFee = driverMaxExtraFee
            }
    return $
      Domain.FarePolicy
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

  toTType Domain.FarePolicy {..} =
    FarePolicyT
      { id = getId id,
        merchantId = toKey merchantId,
        driverMinExtraFee = driverExtraFee.minFee,
        driverMaxExtraFee = driverExtraFee.maxFee,
        ..
      }
