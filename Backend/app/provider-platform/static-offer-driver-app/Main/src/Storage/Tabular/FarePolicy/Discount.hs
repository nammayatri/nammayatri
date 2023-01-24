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

module Storage.Tabular.FarePolicy.Discount where

import qualified Domain.Types.FarePolicy.Discount as Domain
import qualified Domain.Types.FarePolicy.FareProduct as DFareProduct
import qualified Domain.Types.Vehicle as DVeh
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.FareProduct ()
import qualified Storage.Tabular.Merchant as TM
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DiscountT sql=fare_policy_discount
      id Text
      vehicleVariant DVeh.Variant
      merchantId TM.MerchantTId
      fareProductType DFareProduct.FareProductType
      fromDate UTCTime
      toDate UTCTime
      enabled Bool
      discount HighPrecMoney
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey DiscountT where
  type DomainKey DiscountT = Id Domain.Discount
  fromKey (DiscountTKey _id) = Id _id
  toKey (Id id) = DiscountTKey id

instance FromTType DiscountT Domain.Discount where
  fromTType DiscountT {..} = do
    return $
      Domain.Discount
        { id = Id id,
          merchantId = fromKey merchantId,
          discount = roundToIntegral discount,
          ..
        }

instance ToTType DiscountT Domain.Discount where
  toTType Domain.Discount {..} =
    DiscountT
      { id = getId id,
        merchantId = toKey merchantId,
        discount = fromIntegral discount,
        ..
      }
