{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FareProduct
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Domain.Types.FarePolicy as FarePolicy
import Domain.Types.FareProduct
import Domain.Types.Merchant (Merchant)
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.FareProduct

findAllFareProductForVariants ::
  Transactionable m =>
  Id Merchant ->
  Area ->
  m [FareProduct]
findAllFareProductForVariants merchantId area =
  Esq.findAll $ do
    fareProduct <- from $ table @FareProductT
    where_ $
      fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
        &&. fareProduct ^. FareProductArea ==. val area
    return fareProduct

findByMerchantVariantArea ::
  Transactionable m =>
  Id Merchant ->
  Variant ->
  Area ->
  m (Maybe FareProduct)
findByMerchantVariantArea merchantId vehicleVariant area =
  Esq.findOne $ do
    fareProduct <- from $ table @FareProductT
    where_ $
      fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
        &&. fareProduct ^. FareProductArea ==. val area
        &&. fareProduct ^. FareProductVehicleVariant ==. val vehicleVariant
    return fareProduct

updateFareProduct :: Id Merchant -> Variant -> Area -> FlowType -> Id FarePolicy.FarePolicy -> SqlDB ()
updateFareProduct merchantId vehicleVariant area flow farePolicyId =
  Esq.update $ \fareProduct -> do
    set
      fareProduct
      [ FareProductFarePolicyId =. val (toKey farePolicyId)
      ]
    where_ $
      fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
        &&. fareProduct ^. FareProductArea ==. val area
        &&. fareProduct ^. FareProductVehicleVariant ==. val vehicleVariant
        &&. fareProduct ^. FareProductFlow ==. val flow
