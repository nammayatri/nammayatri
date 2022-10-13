module Storage.Queries.FarePolicy.OneWayFarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.FarePolicy.OneWayFarePolicy
import Domain.Types.Organization
import Domain.Types.Vehicle as Vehicle
import qualified Storage.Queries.FarePolicy.OneWayFarePolicy.PerExtraKmRate as QExtraKmRate
import Storage.Queries.FullEntityBuilders (buildFullOneWayFarePolicy)
import Storage.Tabular.FarePolicy.OneWayFarePolicy

findByOrgAndVehicleVariant ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  m (Maybe OneWayFarePolicy)
findByOrgAndVehicleVariant orgId vehicleVariant_ =
  Esq.buildDType $ do
    mbFarePolicy <- Esq.findOne' $ do
      farePolicy <- from $ table @OneWayFarePolicyT
      where_ $
        farePolicy ^. OneWayFarePolicyOrganizationId ==. val (toKey orgId)
          &&. farePolicy ^. OneWayFarePolicyVehicleVariant ==. val vehicleVariant_
      return farePolicy
    mapM buildFullOneWayFarePolicy mbFarePolicy

findAllByOrgId :: Transactionable m => Id Organization -> m [OneWayFarePolicy]
findAllByOrgId orgId =
  Esq.buildDType $ do
    farePolicy <- Esq.findAll' $ do
      farePolicy <- from $ table @OneWayFarePolicyT
      where_ $ farePolicy ^. OneWayFarePolicyOrganizationId ==. val (toKey orgId)
      orderBy [asc $ farePolicy ^. OneWayFarePolicyVehicleVariant]
      return farePolicy
    mapM buildFullOneWayFarePolicy farePolicy

findById :: Transactionable m => Id OneWayFarePolicy -> m (Maybe OneWayFarePolicy)
findById fpId =
  Esq.buildDType $ do
    mbfarePolicy <- Esq.findById' fpId
    mapM buildFullOneWayFarePolicy mbfarePolicy

update :: OneWayFarePolicy -> SqlDB ()
update farePolicy = do
  now <- getCurrentTime
  withFullEntity farePolicy $ \(farePolicyT, perExtraKmRateList, _) -> do
    upsert'
      farePolicyT
      [ OneWayFarePolicyBaseFare =. val (fromIntegral <$> farePolicy.baseFare),
        OneWayFarePolicyNightShiftStart =. val (farePolicy.nightShiftStart),
        OneWayFarePolicyNightShiftEnd =. val (farePolicy.nightShiftEnd),
        OneWayFarePolicyNightShiftRate =. val farePolicy.nightShiftRate,
        OneWayFarePolicyUpdatedAt =. val now
      ]

    QExtraKmRate.deleteAll' farePolicy.organizationId farePolicy.vehicleVariant
    Esq.createMany' perExtraKmRateList
