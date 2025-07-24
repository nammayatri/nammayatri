{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.WalkLegMultimodal (module Storage.Queries.WalkLegMultimodal, module ReExport) where

import qualified Domain.Types.WalkLegMultimodal
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.WalkLegMultimodal as Beam
import Storage.Queries.WalkLegMultimodalExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.WalkLegMultimodal.WalkLegMultimodal -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.WalkLegMultimodal.WalkLegMultimodal] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.WalkLegMultimodal.WalkLegMultimodal -> m (Maybe Domain.Types.WalkLegMultimodal.WalkLegMultimodal))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.WalkLegMultimodal.WalkLegStatus -> Kernel.Types.Id.Id Domain.Types.WalkLegMultimodal.WalkLegMultimodal -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.WalkLegMultimodal.WalkLegMultimodal -> m (Maybe Domain.Types.WalkLegMultimodal.WalkLegMultimodal))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.WalkLegMultimodal.WalkLegMultimodal -> m ())
updateByPrimaryKey (Domain.Types.WalkLegMultimodal.WalkLegMultimodal {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.distanceUnit ((.unit) <$> estimatedDistance),
      Se.Set Beam.estimatedDistance ((.value) <$> estimatedDistance),
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.fromLocationId (Just $ Kernel.Types.Id.getId ((.id) fromLocation)),
      Se.Set Beam.agency (journeyLegInfo >>= (.agency)),
      Se.Set Beam.convenienceCost (Kernel.Prelude.fmap (.convenienceCost) journeyLegInfo),
      Se.Set Beam.isDeleted (journeyLegInfo >>= (.isDeleted)),
      Se.Set Beam.journeyId (Kernel.Prelude.fmap (.journeyId) journeyLegInfo),
      Se.Set Beam.journeyLegOrder (Kernel.Prelude.fmap (.journeyLegOrder) journeyLegInfo),
      Se.Set Beam.onSearchFailed (journeyLegInfo >>= (.onSearchFailed)),
      Se.Set Beam.pricingId (journeyLegInfo >>= (.pricingId)),
      Se.Set Beam.skipBooking (Kernel.Prelude.fmap (.skipBooking) journeyLegInfo),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.status status,
      Se.Set Beam.toLocationId (Kernel.Types.Id.getId <$> (toLocation <&> (.id))),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
