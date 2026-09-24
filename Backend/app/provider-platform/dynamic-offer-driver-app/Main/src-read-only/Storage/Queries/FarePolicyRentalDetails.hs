{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyRentalDetails (module Storage.Queries.FarePolicyRentalDetails, module ReExport) where

import qualified Domain.Types.FarePolicyRentalDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyRentalDetails as Beam
import Storage.Queries.FarePolicyRentalDetailsExtra as ReExport

deleteByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FarePolicyRentalDetails.FarePolicyRentalDetails))
findByFarePolicyId farePolicyId = do findOneWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FarePolicyRentalDetails.FarePolicyRentalDetails))
findByPrimaryKey farePolicyId = do findOneWithKV [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyRentalDetails.FarePolicyRentalDetails -> m ())
updateByPrimaryKey (Domain.Types.FarePolicyRentalDetails.FarePolicyRentalDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.baseFare (Kernel.Prelude.roundToIntegral baseFare),
      Se.Set Beam.baseFareAmount (Kernel.Prelude.Just baseFare),
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.deadKmFare deadKmFare,
      Se.Set Beam.includedKmPerHr includedKmPerHr,
      Se.Set Beam.maxAdditionalKmsLimit maxAdditionalKmsLimit,
      Se.Set Beam.nightShiftCharge nightShiftCharge,
      Se.Set Beam.perExtraKmRate (Kernel.Prelude.roundToIntegral perExtraKmRate),
      Se.Set Beam.perExtraKmRateAmount (Kernel.Prelude.Just perExtraKmRate),
      Se.Set Beam.perExtraMinRate (Kernel.Prelude.roundToIntegral perExtraMinRate),
      Se.Set Beam.perExtraMinRateAmount (Kernel.Prelude.Just perExtraMinRate),
      Se.Set Beam.perHourCharge (Kernel.Prelude.roundToIntegral perHourCharge),
      Se.Set Beam.perHourChargeAmount (Kernel.Prelude.Just perHourCharge),
      Se.Set Beam.plannedPerKmRate (Kernel.Prelude.roundToIntegral plannedPerKmRate),
      Se.Set Beam.plannedPerKmRateAmount (Kernel.Prelude.Just plannedPerKmRate),
      Se.Set Beam.totalAdditionalKmsLimit totalAdditionalKmsLimit,
      Se.Set Beam.freeWaitingTime ((.freeWaitingTime) <$> waitingChargeInfo),
      Se.Set Beam.waitingCharge ((.waitingCharge) <$> waitingChargeInfo)
    ]
    [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]
