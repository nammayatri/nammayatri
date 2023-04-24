{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FareProduct where

import Domain.Types.FareProduct
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.FareProduct

findById ::
  Transactionable m =>
  Id FareProduct ->
  m (Maybe FareProduct)
findById id =
  Esq.findOne $ do
    fareProduct <- from $ table @FareProductT
    where_ $ fareProduct ^. FareProductTId ==. val (toKey id)
    return fareProduct

findBySpecialZoneFareProductIds ::
  Transactionable m =>
  [Id FareProduct] ->
  m [FareProduct]
findBySpecialZoneFareProductIds specialZoneFareProductIds =
  Esq.findAll $ do
    fareProduct <- from $ table @FareProductT
    where_ $ fareProduct ^. FareProductTId `in_` valList (toKey <$> specialZoneFareProductIds)
    return fareProduct

findByMerchantIdAndNotInSpecialZone ::
  Transactionable m =>
  Id Merchant ->
  [Id FareProduct] ->
  m [FareProduct]
findByMerchantIdAndNotInSpecialZone merchantId specialZoneFareProductIds =
  Esq.findAll $ do
    fareProduct <- from $ table @FareProductT
    where_ $
      fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
        &&. not_ (fareProduct ^. FareProductTId `in_` valList (toKey <$> specialZoneFareProductIds))
    return fareProduct

findByMerchantIdAndVehicleVariantAndNotInSpecialZone ::
  Transactionable m =>
  Id Merchant ->
  Vehicle.Variant ->
  [Id FareProduct] ->
  m (Maybe FareProduct)
findByMerchantIdAndVehicleVariantAndNotInSpecialZone merchantId vehicleVariant specialZoneFareProductIds =
  Esq.findOne $ do
    fareProduct <- from $ table @FareProductT
    where_ $
      fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
        &&. fareProduct ^. FareProductVehicleVariant ==. val vehicleVariant
        &&. not_ (fareProduct ^. FareProductTId `in_` valList (toKey <$> specialZoneFareProductIds))
    return fareProduct
