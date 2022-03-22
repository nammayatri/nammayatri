module Storage.Queries.FarePolicy.PerExtraKmRate
  ( Storage.Queries.FarePolicy.PerExtraKmRate.findAll,
    deleteAll,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy.PerExtraKmRate
import Domain.Types.Organization (Organization)
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.FarePolicy.PerExtraKmRate
import Types.Error (FarePolicyError (NoPerExtraKmRate))
import Utils.Common

findAll ::
  ( Transactionable m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id Organization ->
  Vehicle.Variant ->
  m (NonEmpty PerExtraKmRate)
findAll orgId vehicleVariant = do
  rez <- Esq.findAll $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateOrganizationId ==. val (toKey orgId)
        &&. perExtraKmRate ^. PerExtraKmRateVehicleVariant ==. val vehicleVariant
    orderBy [asc $ perExtraKmRate ^. PerExtraKmRateDistanceRangeStart]
    return perExtraKmRate
  noneEmptyRez <- case rez of
    e : es -> pure $ e :| es
    [] -> throwError NoPerExtraKmRate
  return (getDomainPart <$> noneEmptyRez)

deleteAll :: Id Organization -> Vehicle.Variant -> SqlDB ()
deleteAll orgId var =
  delete' $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateOrganizationId ==. val (toKey orgId)
        &&. perExtraKmRate ^. PerExtraKmRateVehicleVariant ==. val var

getDomainPart :: FullPerExtraKmRate -> PerExtraKmRate
getDomainPart (_, _, _, domain) = domain
