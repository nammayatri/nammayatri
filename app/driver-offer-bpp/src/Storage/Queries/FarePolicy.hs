module Storage.Queries.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy
import Domain.Types.Organization
import Domain.Types.Vehicle.Variant (Variant)
import Storage.Tabular.FarePolicy
import Utils.Common

findFarePoliciesByOrg ::
  Transactionable m =>
  Id Organization ->
  m [FarePolicy]
findFarePoliciesByOrg orgId = do
  Esq.findAll $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
    return farePolicy

findFarePolicyByOrgAndVariant ::
  Transactionable m =>
  Id Organization ->
  Variant ->
  m (Maybe FarePolicy)
findFarePolicyByOrgAndVariant orgId variant = do
  Esq.findOne $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
        &&. farePolicy ^. FarePolicyVehicleVariant ==. val variant
    return farePolicy

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById = Esq.findById

updateFarePolicy :: FarePolicy -> SqlDB ()
updateFarePolicy farePolicy = do
  now <- getCurrentTime
  void $
    Esq.update $ \tbl -> do
      set
        tbl
        [ FarePolicyBaseDistancePerKmFare =. val farePolicy.baseDistancePerKmFare,
          FarePolicyBaseDistanceMeters =. val farePolicy.baseDistanceMeters,
          FarePolicyPerExtraKmFare =. val farePolicy.perExtraKmFare,
          FarePolicyDeadKmFare =. val farePolicy.deadKmFare,
          FarePolicyDriverExtraFeeList =. val (PostgresList farePolicy.driverExtraFeeList),
          FarePolicyNightShiftStart =. val farePolicy.nightShiftStart,
          FarePolicyNightShiftEnd =. val farePolicy.nightShiftEnd,
          FarePolicyNightShiftRate =. val farePolicy.nightShiftRate,
          FarePolicyUpdatedAt =. val now
        ]
      where_ $ tbl ^. FarePolicyTId ==. val (toKey farePolicy.id)
