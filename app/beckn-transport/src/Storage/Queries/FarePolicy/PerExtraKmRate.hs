module Storage.Queries.FarePolicy.PerExtraKmRate
  ( Storage.Queries.FarePolicy.PerExtraKmRate.findAll',
    deleteAll',
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization (Organization)
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.FarePolicy.PerExtraKmRate
import Utils.Common

findAll' ::
  ( Transactionable m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id Organization ->
  Vehicle.Variant ->
  DTypeBuilder m [PerExtraKmRateT]
findAll' orgId vehicleVariant = do
  Esq.findAll' $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateOrganizationId ==. val (toKey orgId)
        &&. perExtraKmRate ^. PerExtraKmRateVehicleVariant ==. val vehicleVariant
    orderBy [asc $ perExtraKmRate ^. PerExtraKmRateDistanceRangeStart]
    return perExtraKmRate

deleteAll' :: Id Organization -> Vehicle.Variant -> FullEntitySqlDB ()
deleteAll' orgId var =
  Esq.delete' $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateOrganizationId ==. val (toKey orgId)
        &&. perExtraKmRate ^. PerExtraKmRateVehicleVariant ==. val var
