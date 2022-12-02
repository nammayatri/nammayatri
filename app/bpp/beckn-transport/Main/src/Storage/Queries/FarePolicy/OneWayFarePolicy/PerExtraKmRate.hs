module Storage.Queries.FarePolicy.OneWayFarePolicy.PerExtraKmRate
  ( Storage.Queries.FarePolicy.OneWayFarePolicy.PerExtraKmRate.findAll',
    deleteAll',
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.FarePolicy.OneWayFarePolicy.PerExtraKmRate

findAll' ::
  ( Transactionable m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id Merchant ->
  Vehicle.Variant ->
  DTypeBuilder m [PerExtraKmRateT]
findAll' merchantId vehicleVariant = do
  Esq.findAll' $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateMerchantId ==. val (toKey merchantId)
        &&. perExtraKmRate ^. PerExtraKmRateVehicleVariant ==. val vehicleVariant
    orderBy [asc $ perExtraKmRate ^. PerExtraKmRateDistanceRangeStart]
    return perExtraKmRate

deleteAll' :: Id Merchant -> Vehicle.Variant -> FullEntitySqlDB ()
deleteAll' merchantId var =
  Esq.delete' $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateMerchantId ==. val (toKey merchantId)
        &&. perExtraKmRate ^. PerExtraKmRateVehicleVariant ==. val var
