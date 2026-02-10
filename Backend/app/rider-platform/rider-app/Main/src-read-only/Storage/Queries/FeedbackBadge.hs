{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FeedbackBadge (module Storage.Queries.FeedbackBadge, module ReExport) where

import qualified Domain.Types.FeedbackBadge
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FeedbackBadge as Beam
import Storage.Queries.FeedbackBadgeExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FeedbackBadge.FeedbackBadge -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FeedbackBadge.FeedbackBadge] -> m ())
createMany = traverse_ create

findByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.FeedbackBadge.FeedbackBadge])
findByRiderId riderId = do findAllWithKV [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FeedbackBadge.FeedbackBadge -> m (Maybe Domain.Types.FeedbackBadge.FeedbackBadge))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FeedbackBadge.FeedbackBadge -> m ())
updateByPrimaryKey (Domain.Types.FeedbackBadge.FeedbackBadge {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.badge badge,
      Se.Set Beam.badgeCount badgeCount,
      Se.Set Beam.badgeKey badgeKey,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
