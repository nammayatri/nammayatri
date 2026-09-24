{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicySlabsDetailsSlab (module Storage.Queries.FarePolicySlabsDetailsSlab, module ReExport) where

import qualified Domain.Types.FarePolicySlabsDetailsSlab
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicySlabsDetailsSlab as Beam
import Storage.Queries.FarePolicySlabsDetailsSlabExtra as ReExport

deleteAllByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteAllByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findAllByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m [Domain.Types.FarePolicySlabsDetailsSlab.FarePolicySlabsDetailsSlab])
findAllByFarePolicyId limit offset farePolicyId = do findAllWithOptionsKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId] (Se.Asc Beam.startDistance) limit offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> m (Maybe Domain.Types.FarePolicySlabsDetailsSlab.FarePolicySlabsDetailsSlab))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicySlabsDetailsSlab.FarePolicySlabsDetailsSlab -> m ())
updateByPrimaryKey (Domain.Types.FarePolicySlabsDetailsSlab.FarePolicySlabsDetailsSlab {..}) = do
  updateWithKV
    [ Se.Set Beam.baseFare (Kernel.Prelude.roundToIntegral baseFare),
      Se.Set Beam.baseFareAmount (Kernel.Prelude.Just baseFare),
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.farePolicyId farePolicyId,
      Se.Set Beam.nightShiftCharge nightShiftCharge,
      Se.Set Beam.platformFeeCgst ((.cgst) <$> platformFeeInfo),
      Se.Set Beam.platformFeeCharge ((.platformFeeCharge) <$> platformFeeInfo),
      Se.Set Beam.platformFeeSgst ((.sgst) <$> platformFeeInfo),
      Se.Set Beam.startDistance startDistance,
      Se.Set Beam.freeWatingTime ((.freeWaitingTime) <$> waitingChargeInfo),
      Se.Set Beam.waitingCharge ((.waitingCharge) <$> waitingChargeInfo)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq id]]
