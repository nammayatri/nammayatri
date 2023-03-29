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

module Storage.Tabular.SlabFarePolicy where

import qualified Domain.Types.SlabFarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Centesimal, Meters, Money)
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.Slab"

deriving instance Read Domain.Slab

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SlabFarePolicyT sql=slab_fare_policy
      id Text
      merchantId MerchantTId
      vehicleVariant Variant.Variant

      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe
      nightShiftRate Centesimal Maybe

      maxAllowedTripDistance Meters Maybe
      minAllowedTripDistance Meters Maybe
      serviceCharge Money

      fareSlabs (PostgresList Domain.Slab)
      govtChargesPerc Int Maybe

      createdAt UTCTime
      updatedAt UTCTime
      UniqueSlabFarePolicyId id
      Primary id
      deriving Generic
    |]

instance TEntityKey SlabFarePolicyT where
  type DomainKey SlabFarePolicyT = Id Domain.SlabFarePolicy
  fromKey (SlabFarePolicyTKey _id) = Id _id
  toKey (Id id) = SlabFarePolicyTKey id

instance FromTType SlabFarePolicyT Domain.SlabFarePolicy where
  fromTType SlabFarePolicyT {..} = do
    return $
      Domain.SlabFarePolicy
        { id = Id id,
          merchantId = fromKey merchantId,
          fareSlabs = unPostgresList fareSlabs,
          ..
        }

instance ToTType SlabFarePolicyT Domain.SlabFarePolicy where
  toTType Domain.SlabFarePolicy {..} =
    SlabFarePolicyT
      { id = getId id,
        merchantId = toKey merchantId,
        fareSlabs = PostgresList fareSlabs,
        ..
      }
