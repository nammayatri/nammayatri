module Storage.Queries.FarePolicy.OneWayFarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy.OneWayFarePolicy
import Domain.Types.Organization
import Domain.Types.Vehicle as Vehicle
import qualified Storage.Queries.FarePolicy.OneWayFarePolicy.PerExtraKmRate as QExtraKmRate
import Storage.Queries.FullEntityBuilders (buildFullOneWayFarePolicy)
import Storage.Tabular.FarePolicy.OneWayFarePolicy
import Utils.Common

findOneWayFarePolicyByOrgAndVehicleVariant ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  m (Maybe FarePolicy)
findOneWayFarePolicyByOrgAndVehicleVariant orgId vehicleVariant_ =
  Esq.buildDType $ do
    mbFarePolicy <- Esq.findOne' $ do
      farePolicy <- from $ table @OneWayFarePolicyT
      where_ $
        farePolicy ^. OneWayFarePolicyOrganizationId ==. val (toKey orgId)
          &&. farePolicy ^. OneWayFarePolicyVehicleVariant ==. val vehicleVariant_
      return farePolicy
    mapM buildFullOneWayFarePolicy mbFarePolicy

findOneWayFarePoliciesByOrgId :: Transactionable m => Id Organization -> m [FarePolicy]
findOneWayFarePoliciesByOrgId orgId =
  Esq.buildDType $ do
    farePolicy <- Esq.findAll' $ do
      farePolicy <- from $ table @OneWayFarePolicyT
      where_ $ farePolicy ^. OneWayFarePolicyOrganizationId ==. val (toKey orgId)
      orderBy [asc $ farePolicy ^. OneWayFarePolicyVehicleVariant]
      return farePolicy
    mapM buildFullOneWayFarePolicy farePolicy

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById fpId =
  Esq.buildDType $ do
    mbfarePolicy <- Esq.findById' fpId
    mapM buildFullOneWayFarePolicy mbfarePolicy

updateOneWayFarePolicy :: FarePolicy -> SqlDB ()
updateOneWayFarePolicy farePolicy = do
  now <- getCurrentTime
  withFullEntity farePolicy $ \(farePolicyT, _, perExtraKmRateList) -> do
    upsert'
      farePolicyT
      [ OneWayFarePolicyBaseFare =. val (fromRational <$> farePolicy.baseFare),
        OneWayFarePolicyNightShiftStart =. val (farePolicy.nightShiftStart),
        OneWayFarePolicyNightShiftEnd =. val (farePolicy.nightShiftEnd),
        OneWayFarePolicyNightShiftRate =. val (fromRational <$> farePolicy.nightShiftRate),
        OneWayFarePolicyUpdatedAt =. val now
      ]

    QExtraKmRate.deleteAll' farePolicy.organizationId farePolicy.vehicleVariant
    Esq.createMany' perExtraKmRateList
