module Storage.Queries.FarePolicy.Discount
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.FarePolicy.Discount
import Domain.Types.Organization (Organization)
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.FarePolicy.Discount

create :: Discount -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Discount ->
  m (Maybe Discount)
findById = Esq.findById

findAllByOrgIdAndVariant ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  m [Discount]
findAllByOrgIdAndVariant orgId vehicleVariant =
  Esq.buildDType $
    fmap extractSolidType
      <$> Storage.Queries.FarePolicy.Discount.findAllByOrgIdAndVariant' orgId vehicleVariant

findAllByOrgIdAndVariant' ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  DTypeBuilder m [DiscountT]
findAllByOrgIdAndVariant' orgId vehicleVariant =
  Esq.findAll' $ do
    discount <- from $ table @DiscountT
    where_ $
      discount ^. DiscountOrganizationId ==. val (toKey orgId)
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
