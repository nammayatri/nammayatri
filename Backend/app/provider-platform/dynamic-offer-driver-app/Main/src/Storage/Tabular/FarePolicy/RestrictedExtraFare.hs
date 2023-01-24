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

module Storage.Tabular.FarePolicy.RestrictedExtraFare where

import qualified Domain.Types.FarePolicy.RestrictedExtraFare as Domain
import qualified Domain.Types.Vehicle.Variant as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Meters, Money)
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RestrictedExtraFareT sql=restricted_extra_fare
      id Text
      merchantId MerchantTId
      vehicleVariant Vehicle.Variant
      minTripDistance Meters
      driverMaxExtraFare Money
      Primary id
      deriving Generic
    |]

instance TEntityKey RestrictedExtraFareT where
  type DomainKey RestrictedExtraFareT = Id Domain.RestrictedExtraFare
  fromKey (RestrictedExtraFareTKey _id) = Id _id
  toKey (Id id) = RestrictedExtraFareTKey id

instance FromTType RestrictedExtraFareT Domain.RestrictedExtraFare where
  fromTType RestrictedExtraFareT {..} = do
    return $
      Domain.RestrictedExtraFare
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType RestrictedExtraFareT Domain.RestrictedExtraFare where
  toTType Domain.RestrictedExtraFare {..} =
    RestrictedExtraFareT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }
