{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy
import Domain.Types.Merchant
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.FarePolicy

findAllByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m [FarePolicy]
findAllByMerchantId merchantId = do
  Esq.findAll $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
    return farePolicy

findByMerchantIdAndVariant ::
  Transactionable m =>
  Id Merchant ->
  Variant ->
  m (Maybe FarePolicy)
findByMerchantIdAndVariant merchantId variant = do
  Esq.findOne $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
        &&. farePolicy ^. FarePolicyVehicleVariant ==. val variant
    return farePolicy

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById = Esq.findById

update :: FarePolicy -> SqlDB ()
update farePolicy = do
  now <- getCurrentTime
  void $
    Esq.update $ \tbl -> do
      set
        tbl
        [ FarePolicyDriverMinExtraFee =. val (farePolicy.driverExtraFeeBounds <&> (.minFee)),
          FarePolicyDriverMaxExtraFee =. val (farePolicy.driverExtraFeeBounds <&> (.maxFee)),
          FarePolicyNightShiftStart =. val (farePolicy.nightShiftBounds <&> (.nightShiftStart)),
          FarePolicyNightShiftEnd =. val (farePolicy.nightShiftBounds <&> (.nightShiftEnd)),
          FarePolicyUpdatedAt =. val now
        ]
      where_ $ tbl ^. FarePolicyTId ==. val (toKey farePolicy.id)
