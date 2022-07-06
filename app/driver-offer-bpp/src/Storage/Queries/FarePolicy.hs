module Storage.Queries.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.FarePolicy
import Domain.Types.Organization
import Storage.Tabular.FarePolicy
import Utils.Common

findFarePolicyByOrg ::
  Transactionable m =>
  Id Organization ->
  m (Maybe FarePolicy)
findFarePolicyByOrg orgId = do
  Esq.findOne $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
    return farePolicy

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById = Esq.findById

updateFarePolicy :: FarePolicy -> SqlDB ()
updateFarePolicy farePolicy = do
  now <- getCurrentTime
  void $
    Esq.upsert
      farePolicy
      [ FarePolicyFareForPickup =. val (amountToDouble farePolicy.fareForPickup),
        FarePolicyFarePerKm =. val (amountToDouble farePolicy.farePerKm),
        FarePolicyNightShiftStart =. val (farePolicy.nightShiftStart),
        FarePolicyNightShiftEnd =. val (farePolicy.nightShiftEnd),
        FarePolicyNightShiftRate =. val (amountToDouble <$> farePolicy.nightShiftRate),
        FarePolicyUpdatedAt =. val now
      ]
