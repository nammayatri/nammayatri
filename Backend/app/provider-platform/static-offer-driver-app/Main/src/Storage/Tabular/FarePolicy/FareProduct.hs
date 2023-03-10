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

module Storage.Tabular.FarePolicy.FareProduct where

import qualified Domain.Types.FarePolicy.FareProduct as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Merchant as TM

derivePersistField "Domain.FareProductType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FareProductT sql=fare_product
      id Text
      merchantId TM.MerchantTId
      productType Domain.FareProductType sql=type
      createdAt UTCTime
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
          _type = productType,
          ..
        }

instance ToTType FareProductT Domain.FareProduct where
  toTType Domain.FareProduct {..} =
    FareProductT
      { id = getId id,
        merchantId = toKey merchantId,
        productType = _type,
        ..
      }
