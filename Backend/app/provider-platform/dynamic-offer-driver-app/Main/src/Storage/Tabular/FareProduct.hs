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

module Storage.Tabular.FareProduct where

import qualified Domain.Types.FareProduct as Domain
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id (Id (..))
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.FarePolicyType"
derivePersistField "Domain.FlowType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FareProductT sql=fare_product
      id Text
      merchantId MerchantTId
      vehicleVariant Vehicle.Variant
      farePolicyType Domain.FarePolicyType
      flowType Domain.FlowType
      Primary id
      deriving Generic
    |]

instance TEntityKey FareProductT where
  type DomainKey FareProductT = Id Domain.FareProduct
  fromKey (FareProductTKey _id) = Id _id
  toKey (Id id) = FareProductTKey id

instance FromTType FareProductT Domain.FareProduct where
  fromTType FareProductT {..} = do
    return $
      Domain.FareProduct
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType FareProductT Domain.FareProduct where
  toTType Domain.FareProduct {..} =
    FareProductT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }
