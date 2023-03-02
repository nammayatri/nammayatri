{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy.Discount
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy.Discount
import Domain.Types.Merchant (Merchant)
import Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.FarePolicy.Discount

create :: Discount -> SqlDB m ()
create = Esq.create

findById ::
  forall m ma.
  Transactionable ma m =>
  Id Discount ->
  Proxy ma ->
  m (Maybe Discount)
findById discId _ = Esq.findById @m @ma discId

findAllByMerchantIdAndVariant ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Vehicle.Variant ->
  Proxy ma ->
  m [Discount]
findAllByMerchantIdAndVariant merchantId vehicleVariant proxy =
  Esq.buildDType $
    fmap (extractSolidType @Discount)
      <$> Storage.Queries.FarePolicy.Discount.findAllByMerchantIdAndVariant' merchantId vehicleVariant proxy

findAllByMerchantIdAndVariant' ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Vehicle.Variant ->
  Proxy ma ->
  DTypeBuilder m [DiscountT]
findAllByMerchantIdAndVariant' merchantId vehicleVariant _ =
  Esq.findAll' @m @ma $ do
    discount <- from $ table @DiscountT
    where_ $
      discount ^. DiscountMerchantId ==. val (toKey merchantId)
        &&. discount ^. DiscountVehicleVariant ==. val vehicleVariant
    return discount

update :: Discount -> SqlDB m ()
update disc = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DiscountFromDate =. val disc.fromDate,
        DiscountToDate =. val disc.toDate,
        DiscountEnabled =. val disc.enabled,
        DiscountDiscount =. val (fromIntegral disc.discount),
        DiscountUpdatedAt =. val now
      ]
    where_ $ tbl ^. DiscountId ==. val (getId disc.id)

deleteById :: Id Discount -> SqlDB m ()
deleteById = deleteByKey @DiscountT
