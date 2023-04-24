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

module Storage.Tabular.SpecialZoneLink where

import qualified Domain.Types.SpecialZoneLink as Domain
import qualified Domain.Types.Vehicle.Variant as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id (Id (..))
import Storage.Tabular.FareProduct (FareProductTId)
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.PickupOrDropType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SpecialZoneLinkT sql=special_zone_link
      id Text
      merchantId MerchantTId
      fareProductId FareProductTId
      specialZoneId Text
      vehicleVariant Vehicle.Variant
      pickupOrDrop Domain.PickupOrDropType
      Primary id
      deriving Generic
    |]

instance TEntityKey SpecialZoneLinkT where
  type DomainKey SpecialZoneLinkT = Id Domain.SpecialZoneLink
  fromKey (SpecialZoneLinkTKey _id) = Id _id
  toKey (Id id) = SpecialZoneLinkTKey id

instance FromTType SpecialZoneLinkT Domain.SpecialZoneLink where
  fromTType SpecialZoneLinkT {..} = do
    return $
      Domain.SpecialZoneLink
        { id = Id id,
          merchantId = fromKey merchantId,
          fareProductId = fromKey fareProductId,
          ..
        }

instance ToTType SpecialZoneLinkT Domain.SpecialZoneLink where
  toTType Domain.SpecialZoneLink {..} =
    SpecialZoneLinkT
      { id = getId id,
        merchantId = toKey merchantId,
        fareProductId = toKey fareProductId,
        ..
      }
