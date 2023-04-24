{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SpecialZoneLink where

import Domain.Types.Merchant
import Domain.Types.SpecialZoneLink
import qualified Domain.Types.Vehicle as DVehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Types.SpecialLocation as DSpecialLocation
import Storage.Tabular.SpecialZoneLink

findByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m [SpecialZoneLink]
findByMerchantId merchantId =
  Esq.findAll $ do
    specialZoneLink <- from $ table @SpecialZoneLinkT
    where_ $ specialZoneLink ^. SpecialZoneLinkMerchantId ==. val (toKey merchantId)
    return specialZoneLink

findByMerchantIdAndSpecialZoneIdForPickupOrDrop ::
  Transactionable m =>
  Id Merchant ->
  Id DSpecialLocation.SpecialLocation ->
  PickupOrDropType ->
  m [SpecialZoneLink]
findByMerchantIdAndSpecialZoneIdForPickupOrDrop merchantId specialZoneId pickupOrDrop =
  Esq.findAll $ do
    specialZoneLink <- from $ table @SpecialZoneLinkT
    where_ $
      specialZoneLink ^. SpecialZoneLinkMerchantId ==. val (toKey merchantId)
        &&. specialZoneLink ^. SpecialZoneLinkSpecialZoneId ==. val specialZoneId.getId
        &&. specialZoneLink ^. SpecialZoneLinkPickupOrDrop ==. val pickupOrDrop
    return specialZoneLink

findByMerchantIdAndSpecialZoneIdForPickupOrDropAndVehicleVariant ::
  Transactionable m =>
  Id Merchant ->
  Id DSpecialLocation.SpecialLocation ->
  PickupOrDropType ->
  DVehicle.Variant ->
  m (Maybe SpecialZoneLink)
findByMerchantIdAndSpecialZoneIdForPickupOrDropAndVehicleVariant merchantId specialZoneId pickupOrDrop vehicleVariant =
  Esq.findOne $ do
    specialZoneLink <- from $ table @SpecialZoneLinkT
    where_ $
      specialZoneLink ^. SpecialZoneLinkMerchantId ==. val (toKey merchantId)
        &&. specialZoneLink ^. SpecialZoneLinkSpecialZoneId ==. val specialZoneId.getId
        &&. specialZoneLink ^. SpecialZoneLinkPickupOrDrop ==. val pickupOrDrop
        &&. specialZoneLink ^. SpecialZoneLinkVehicleVariant ==. val vehicleVariant
    return specialZoneLink
