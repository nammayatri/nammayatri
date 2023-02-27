{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.RentalFarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy.RentalFarePolicy
import qualified Domain.Types.FarePolicy.RentalFarePolicy as Domain
import Domain.Types.Merchant
import Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (Hours, Kilometers)
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.RentalFarePolicy

create ::
  Domain.RentalFarePolicy ->
  SqlDB ()
create = Esq.create

-- it's possible to find deleted fare policies only by their id.
-- other function return only not deleted fare policies
-- (RentalFarePolicyDeleted ==. val False)

findById :: Transactionable m => Id RentalFarePolicy -> m (Maybe RentalFarePolicy)
findById = Esq.findById

findAllByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m [RentalFarePolicy]
findAllByMerchantId merchantId = do
  Esq.findAll $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyMerchantId ==. val (toKey merchantId)
        &&. rentalFarePolicy ^. RentalFarePolicyDeleted ==. val False
    return rentalFarePolicy

markAllAsDeleted ::
  Id Merchant ->
  SqlDB ()
markAllAsDeleted merchantId = Esq.update $ \rentalFp -> do
  set
    rentalFp
    [RentalFarePolicyDeleted =. val True]
  where_ $ rentalFp ^. RentalFarePolicyMerchantId ==. val (toKey merchantId)

findByOffer ::
  Transactionable m =>
  Id Merchant ->
  Vehicle.Variant ->
  Kilometers ->
  Hours ->
  m (Maybe RentalFarePolicy)
findByOffer merchantId vehicleVariant_ baseDistance baseDuration = do
  Esq.findOne $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyMerchantId ==. val (toKey merchantId)
        &&. rentalFarePolicy ^. RentalFarePolicyVehicleVariant ==. val vehicleVariant_
        &&. rentalFarePolicy ^. RentalFarePolicyBaseDistance ==. val baseDistance.getKilometers
        &&. rentalFarePolicy ^. RentalFarePolicyBaseDuration ==. val baseDuration.getHours
    return rentalFarePolicy
