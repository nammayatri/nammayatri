module Storage.Queries.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy
import Domain.Types.FarePolicy.PerExtraKmRate (PerExtraKmRate)
import Domain.Types.Organization
import qualified Storage.Queries.FarePolicy.PerExtraKmRate as QExtraKmRate
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
    upsert'
      farePolicy
      [ FarePolicyBaseFare =. val (fromRational <$> farePolicy.baseFare),
        FarePolicyNightShiftStart =. val (farePolicy.nightShiftStart),
        FarePolicyNightShiftEnd =. val (farePolicy.nightShiftEnd),
        FarePolicyNightShiftRate =. val (fromRational <$> farePolicy.nightShiftRate),
        FarePolicyUpdatedAt =. val now
      ]
  QExtraKmRate.deleteAll farePolicy.organizationId
  perExtraKmRateList <- mapM (buildPerExtraKmRate farePolicy) farePolicy.perExtraKmRateList
  create' `mapM_` perExtraKmRateList
  where
    buildPerExtraKmRate FarePolicy {..} perExtraKmRate = do
      uuid <- generateGUID
      return (Id uuid :: Id PerExtraKmRate, organizationId, perExtraKmRate)
