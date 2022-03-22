module Storage.Queries.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy
import Domain.Types.FarePolicy.PerExtraKmRate (PerExtraKmRate)
import Domain.Types.Organization
import Domain.Types.Vehicle as Vehicle
import qualified Storage.Queries.FarePolicy.PerExtraKmRate as QExtraKmRate
import Storage.Tabular.FarePolicy
import Utils.Common

findFarePolicyByOrgAndVehicleVariant ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  m (Maybe FarePolicy)
findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant_ = do
  Esq.findOne $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
        &&. farePolicy ^. FarePolicyVehicleVariant ==. val vehicleVariant_
    return farePolicy

findFarePoliciesByOrgId :: Transactionable m => Id Organization -> m [FarePolicy]
findFarePoliciesByOrgId orgId = do
  Esq.findAll $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $ farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
    orderBy [asc $ farePolicy ^. FarePolicyVehicleVariant]
    return farePolicy

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById = Esq.findById

updateFarePolicy :: FarePolicy -> SqlDB ()
updateFarePolicy farePolicy = do
  now <- getCurrentTime
  void $
    upsert'
      farePolicy
      [ FarePolicyBaseFare =. val (fromRational <$> farePolicy.baseFare),
        FarePolicyNightShiftStart =. val (farePolicy.nightShiftStart),
        FarePolicyNightShiftEnd =. val (farePolicy.nightShiftEnd),
        FarePolicyNightShiftRate =. val (fromRational <$> farePolicy.nightShiftRate),
        FarePolicyUpdatedAt =. val now
      ]
  QExtraKmRate.deleteAll farePolicy.organizationId farePolicy.vehicleVariant
  perExtraKmRateList <- mapM (buildPerExtraKmRate farePolicy) farePolicy.perExtraKmRateList
  create' `mapM_` perExtraKmRateList
  where
    buildPerExtraKmRate FarePolicy {..} perExtraKmRate = do
      uuid <- generateGUID
      return (Id uuid :: Id PerExtraKmRate, organizationId, vehicleVariant, perExtraKmRate)
