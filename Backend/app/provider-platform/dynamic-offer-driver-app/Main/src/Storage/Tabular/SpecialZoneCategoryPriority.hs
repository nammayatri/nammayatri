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

module Storage.Tabular.SpecialZoneCategoryPriority where

import qualified Domain.Types.SpecialZoneCategoryPriority as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id (Id (..))
import Storage.Tabular.Merchant (MerchantTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SpecialZoneCategoryPriorityT sql=special_zone_category_priority
      id Text
      category Text
      merchantId MerchantTId
      pickupPriority Int
      dropPriority Int
      Primary id
      deriving Generic
    |]

instance TEntityKey SpecialZoneCategoryPriorityT where
  type DomainKey SpecialZoneCategoryPriorityT = Id Domain.SpecialZoneCategoryPriority
  fromKey (SpecialZoneCategoryPriorityTKey _id) = Id _id
  toKey (Id id) = SpecialZoneCategoryPriorityTKey id

instance FromTType SpecialZoneCategoryPriorityT Domain.SpecialZoneCategoryPriority where
  fromTType SpecialZoneCategoryPriorityT {..} = do
    return $
      Domain.SpecialZoneCategoryPriority
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType SpecialZoneCategoryPriorityT Domain.SpecialZoneCategoryPriority where
  toTType Domain.SpecialZoneCategoryPriority {..} =
    SpecialZoneCategoryPriorityT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }
