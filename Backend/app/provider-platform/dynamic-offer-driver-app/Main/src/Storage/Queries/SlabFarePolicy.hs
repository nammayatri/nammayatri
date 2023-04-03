{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SlabFarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.SlabFarePolicy
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.SlabFarePolicy

findAllByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m [SlabFarePolicy]
findAllByMerchantId merchantId = do
  Esq.findAll $ do
    farePolicy <- from $ table @SlabFarePolicyT
    where_ $
      farePolicy ^. SlabFarePolicyMerchantId ==. val (toKey merchantId)
    return farePolicy

findByMerchantIdAndVariant ::
  Transactionable m =>
  Id Merchant ->
  Variant ->
  m (Maybe SlabFarePolicy)
findByMerchantIdAndVariant merchantId variant = do
  Esq.findOne $ do
    farePolicy <- from $ table @SlabFarePolicyT
    where_ $
      farePolicy ^. SlabFarePolicyMerchantId ==. val (toKey merchantId)
        &&. farePolicy ^. SlabFarePolicyVehicleVariant ==. val variant
    return farePolicy

findById :: Transactionable m => Id SlabFarePolicy -> m (Maybe SlabFarePolicy)
findById = Esq.findById
