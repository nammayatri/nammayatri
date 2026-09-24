{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyAmbulanceDetailsSlab (module Storage.Queries.FarePolicyAmbulanceDetailsSlab, module ReExport) where

import qualified Domain.Types.FarePolicyAmbulanceDetailsSlab
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyAmbulanceDetailsSlab as Beam
import Storage.Queries.FarePolicyAmbulanceDetailsSlabExtra as ReExport

deleteByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findAllByFarePolicyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m [Domain.Types.FarePolicyAmbulanceDetailsSlab.FarePolicyAmbulanceDetailsSlab])
findAllByFarePolicyId limit offset farePolicyId = do findAllWithOptionsKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId] (Se.Asc Beam.vehicleAge) limit offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> m (Maybe Domain.Types.FarePolicyAmbulanceDetailsSlab.FarePolicyAmbulanceDetailsSlab))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyAmbulanceDetailsSlab.FarePolicyAmbulanceDetailsSlab -> m ())
updateByPrimaryKey (Domain.Types.FarePolicyAmbulanceDetailsSlab.FarePolicyAmbulanceDetailsSlab {..}) = do
  updateWithKV
    [ Se.Set Beam.baseDistance baseDistance,
      Se.Set Beam.baseFare baseFare,
      Se.Set Beam.currency currency,
      Se.Set Beam.farePolicyId farePolicyId,
      Se.Set Beam.nightShiftCharge nightShiftCharge,
      Se.Set Beam.perKmRate perKmRate,
      Se.Set Beam.platformFeeCgst ((.cgst) <$> platformFeeInfo),
      Se.Set Beam.platformFeeCharge ((.platformFeeCharge) <$> platformFeeInfo),
      Se.Set Beam.platformFeeSgst ((.sgst) <$> platformFeeInfo),
      Se.Set Beam.vehicleAge vehicleAge,
      Se.Set Beam.freeWaitingTime ((.freeWaitingTime) <$> waitingChargeInfo),
      Se.Set Beam.waitingCharge ((.waitingCharge) <$> waitingChargeInfo)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq id]]
