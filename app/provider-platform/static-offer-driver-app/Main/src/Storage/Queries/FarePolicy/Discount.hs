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

create :: Discount -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Discount ->
  m (Maybe Discount)
findById = Esq.findById

findAllByMerchantIdAndVariant ::
  Transactionable m =>
  Id Merchant ->
  Vehicle.Variant ->
  m [Discount]
findAllByMerchantIdAndVariant merchantId vehicleVariant =
  Esq.buildDType $
    fmap (extractSolidType @Discount)
      <$> Storage.Queries.FarePolicy.Discount.findAllByMerchantIdAndVariant' merchantId vehicleVariant

findAllByMerchantIdAndVariant' ::
  Transactionable m =>
  Id Merchant ->
  Vehicle.Variant ->
  DTypeBuilder m [DiscountT]
findAllByMerchantIdAndVariant' merchantId vehicleVariant =
  Esq.findAll' $ do
    discount <- from $ table @DiscountT
    where_ $
      discount ^. DiscountMerchantId ==. val (toKey merchantId)
        &&. discount ^. DiscountVehicleVariant ==. val vehicleVariant
    return discount

update :: Discount -> SqlDB ()
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

deleteById :: Id Discount -> SqlDB ()
deleteById = deleteByKey @DiscountT
