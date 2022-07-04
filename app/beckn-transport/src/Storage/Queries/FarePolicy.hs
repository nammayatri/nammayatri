module Storage.Queries.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy
import Domain.Types.Organization
import Domain.Types.Vehicle as Vehicle
import qualified Storage.Queries.FarePolicy.PerExtraKmRate as QExtraKmRate
import Storage.Queries.FullEntityBuilders (buildFullFarePolicy)
import Storage.Tabular.FarePolicy
import Utils.Common

findFarePolicyByOrgAndVehicleVariant ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  m (Maybe FarePolicy)
findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant_ =
  Esq.buildDType $ do
    mbFarePolicy <- Esq.findOne' $ do
      farePolicy <- from $ table @FarePolicyT
      where_ $
        farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
          &&. farePolicy ^. FarePolicyVehicleVariant ==. val vehicleVariant_
      return farePolicy
    mapM buildFullFarePolicy mbFarePolicy

findFarePoliciesByOrgId :: Transactionable m => Id Organization -> m [FarePolicy]
findFarePoliciesByOrgId orgId =
  Esq.buildDType $ do
    farePolicy <- Esq.findAll' $ do
      farePolicy <- from $ table @FarePolicyT
      where_ $ farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
      orderBy [asc $ farePolicy ^. FarePolicyVehicleVariant]
      return farePolicy
    mapM buildFullFarePolicy farePolicy

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById fpId =
  Esq.buildDType $ do
    mbfarePolicy <- Esq.findById' fpId
    mapM buildFullFarePolicy mbfarePolicy

updateFarePolicy :: FarePolicy -> SqlDB ()
updateFarePolicy farePolicy = do
  now <- getCurrentTime
  withFullEntity farePolicy $ \(farePolicyT, _, perExtraKmRateList) -> do
    upsert'
      farePolicyT
      [ FarePolicyBaseFare =. val (fromRational <$> farePolicy.baseFare),
        FarePolicyNightShiftStart =. val (farePolicy.nightShiftStart),
        FarePolicyNightShiftEnd =. val (farePolicy.nightShiftEnd),
        FarePolicyNightShiftRate =. val (fromRational <$> farePolicy.nightShiftRate),
        FarePolicyUpdatedAt =. val now
      ]

    QExtraKmRate.deleteAll' farePolicy.organizationId farePolicy.vehicleVariant
    Esq.createMany' perExtraKmRateList
