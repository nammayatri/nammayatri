{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyRentalDetailsDistanceBuffers (module Storage.Queries.FarePolicyRentalDetailsDistanceBuffers, module ReExport) where

import qualified Domain.Types.FarePolicyRentalDetailsDistanceBuffers
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyRentalDetailsDistanceBuffers as Beam
import Storage.Queries.FarePolicyRentalDetailsDistanceBuffersExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyRentalDetailsDistanceBuffers.FarePolicyRentalDetailsDistanceBuffers -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FarePolicyRentalDetailsDistanceBuffers.FarePolicyRentalDetailsDistanceBuffers] -> m ())
createMany = traverse_ create

deleteByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findAllByFarePolicyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m [Domain.Types.FarePolicyRentalDetailsDistanceBuffers.FarePolicyRentalDetailsDistanceBuffers])
findAllByFarePolicyId limit offset farePolicyId = do findAllWithOptionsKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId] (Se.Asc Beam.rideDuration) limit offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FarePolicyRentalDetailsDistanceBuffers.FarePolicyRentalDetailsDistanceBuffers))
findByPrimaryKey farePolicyId = do findOneWithKV [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyRentalDetailsDistanceBuffers.FarePolicyRentalDetailsDistanceBuffers -> m ())
updateByPrimaryKey (Domain.Types.FarePolicyRentalDetailsDistanceBuffers.FarePolicyRentalDetailsDistanceBuffers {..}) = do
  updateWithKV
    [ Se.Set Beam.bufferKms bufferKms,
      Se.Set Beam.bufferMeters bufferMeters,
      Se.Set Beam.rideDuration rideDuration
    ]
    [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]
