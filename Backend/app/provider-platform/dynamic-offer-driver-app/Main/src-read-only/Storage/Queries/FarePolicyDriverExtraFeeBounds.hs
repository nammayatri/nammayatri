{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyDriverExtraFeeBounds (module Storage.Queries.FarePolicyDriverExtraFeeBounds, module ReExport) where

import qualified Domain.Types.FarePolicyDriverExtraFeeBounds
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyDriverExtraFeeBounds as Beam
import Storage.Queries.FarePolicyDriverExtraFeeBoundsExtra as ReExport

deleteAllByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteAllByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findAllByFarePolicyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m [Domain.Types.FarePolicyDriverExtraFeeBounds.FarePolicyDriverExtraFeeBounds])
findAllByFarePolicyId limit offset farePolicyId = do findAllWithOptionsKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId] (Se.Asc Beam.startDistance) limit offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> m (Maybe Domain.Types.FarePolicyDriverExtraFeeBounds.FarePolicyDriverExtraFeeBounds))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyDriverExtraFeeBounds.FarePolicyDriverExtraFeeBounds -> m ())
updateByPrimaryKey (Domain.Types.FarePolicyDriverExtraFeeBounds.FarePolicyDriverExtraFeeBounds {..}) = do
  updateWithKV
    [ Se.Set Beam.defaultStepFee (Kernel.Prelude.roundToIntegral defaultStepFee),
      Se.Set Beam.defaultStepFeeAmount (Kernel.Prelude.Just defaultStepFee),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.farePolicyId farePolicyId,
      Se.Set Beam.maxFee (Kernel.Prelude.roundToIntegral maxFee),
      Se.Set Beam.maxFeeAmount (Kernel.Prelude.Just maxFee),
      Se.Set Beam.minFee (Kernel.Prelude.roundToIntegral minFee),
      Se.Set Beam.minFeeAmount (Kernel.Prelude.Just minFee),
      Se.Set Beam.startDistance startDistance,
      Se.Set Beam.stepFee (Kernel.Prelude.roundToIntegral stepFee),
      Se.Set Beam.stepFeeAmount (Kernel.Prelude.Just stepFee)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq id]]
