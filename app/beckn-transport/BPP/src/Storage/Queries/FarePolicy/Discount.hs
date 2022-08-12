module Storage.Queries.FarePolicy.Discount where

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

findAll ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  m [Discount]
findAll orgId vehicleVariant =
  Esq.findAll $ do
    discount <- from $ table @DiscountT
    where_ $
      discount ^. DiscountOrganizationId ==. val (toKey orgId)
        &&. discount ^. DiscountVehicleVariant ==. val vehicleVariant
    return discount

findAll' ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  DTypeBuilder m [DiscountT]
findAll' orgId vehicleVariant =
  Esq.findAll' $ do
    discount <- from $ table @DiscountT
    where_ $
      discount ^. DiscountOrganizationId ==. val (toKey orgId)
        &&. discount ^. DiscountVehicleVariant ==. val vehicleVariant
    return discount

update :: Id Discount -> Discount -> SqlDB ()
update discId disc = do
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
    where_ $ tbl ^. DiscountId ==. val (getId discId)

deleteById :: Id Discount -> SqlDB ()
deleteById = deleteByKey @DiscountT
