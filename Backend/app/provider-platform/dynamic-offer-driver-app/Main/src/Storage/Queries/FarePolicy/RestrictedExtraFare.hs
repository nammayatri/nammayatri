{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.RestrictedExtraFare where

import qualified Domain.Types.FarePolicy.RestrictedExtraFare as Domain
import Domain.Types.Merchant
import qualified Domain.Types.Vehicle.Variant as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.RestrictedExtraFare

create :: Domain.RestrictedExtraFare -> SqlDB m ()
create = Esq.create

findMaxExtraFareByMerchantAndVehicle :: forall m ma. (Transactionable ma m) => Id Merchant -> Vehicle.Variant -> Proxy ma -> m [Domain.RestrictedExtraFare]
findMaxExtraFareByMerchantAndVehicle merchantId vehicleVariant _ = do
  findAll @m @ma $ do
    restrictedExtraFare <- Esq.from $ table @RestrictedExtraFareT
    where_ $
      restrictedExtraFare ^. RestrictedExtraFareMerchantId ==. val (toKey merchantId)
        &&. restrictedExtraFare ^. RestrictedExtraFareVehicleVariant ==. val vehicleVariant
    orderBy [desc (restrictedExtraFare ^. RestrictedExtraFareMinTripDistance)]
    return restrictedExtraFare

findMaxExtraFareByMerchant :: forall m ma. (Transactionable ma m) => Id Merchant -> Proxy ma -> m [Domain.RestrictedExtraFare]
findMaxExtraFareByMerchant merchantId _ = do
  findAll @m @ma $ do
    restrictedExtraFare <- Esq.from $ table @RestrictedExtraFareT
    where_ $
      restrictedExtraFare ^. RestrictedExtraFareMerchantId ==. val (toKey merchantId)
    orderBy [desc (restrictedExtraFare ^. RestrictedExtraFareMinTripDistance)]
    return restrictedExtraFare
