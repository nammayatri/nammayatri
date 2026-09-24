{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyProgressiveDetails (module Storage.Queries.FarePolicyProgressiveDetails, module ReExport) where

import qualified Domain.Types.FarePolicyProgressiveDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyProgressiveDetails as Beam
import Storage.Queries.FarePolicyProgressiveDetailsExtra as ReExport

deleteByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FarePolicyProgressiveDetails.FarePolicyProgressiveDetails))
findByFarePolicyId farePolicyId = do findOneWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FarePolicyProgressiveDetails.FarePolicyProgressiveDetails))
findByPrimaryKey farePolicyId = do findOneWithKV [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyProgressiveDetails.FarePolicyProgressiveDetails -> m ())
updateByPrimaryKey (Domain.Types.FarePolicyProgressiveDetails.FarePolicyProgressiveDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.baseDistance baseDistance,
      Se.Set Beam.baseFare (Kernel.Prelude.roundToIntegral baseFare),
      Se.Set Beam.baseFareAmount (Kernel.Prelude.Just baseFare),
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.deadKmFare (Kernel.Prelude.roundToIntegral deadKmFare),
      Se.Set Beam.deadKmFareAmount (Kernel.Prelude.Just deadKmFare),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.nightShiftCharge nightShiftCharge,
      Se.Set Beam.perMinRateDurationBasis perMinRateDurationBasis,
      Se.Set Beam.pickupChargesMax (Kernel.Prelude.Just $ Kernel.Prelude.roundToIntegral ((.pickupChargesMax) pickupCharges)),
      Se.Set Beam.pickupChargesMaxAmount (Kernel.Prelude.Just ((.pickupChargesMax) pickupCharges)),
      Se.Set Beam.pickupChargesMin (Kernel.Prelude.Just $ Kernel.Prelude.roundToIntegral ((.pickupChargesMin) pickupCharges)),
      Se.Set Beam.pickupChargesMinAmount (Kernel.Prelude.Just ((.pickupChargesMin) pickupCharges)),
      Se.Set Beam.freeWatingTime ((.freeWaitingTime) <$> waitingChargeInfo),
      Se.Set Beam.waitingCharge ((.waitingCharge) <$> waitingChargeInfo)
    ]
    [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]
