{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.FeedbackBadge (module Storage.Queries.FeedbackBadge, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.FeedbackBadgeExtra as ReExport
import qualified Domain.Types.FeedbackBadge
import qualified Storage.Beam.FeedbackBadge as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FeedbackBadge.FeedbackBadge -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FeedbackBadge.FeedbackBadge] -> m ())
createMany = traverse_ create
findByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.FeedbackBadge.FeedbackBadge]))
findByRiderId riderId = do findAllWithKV [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FeedbackBadge.FeedbackBadge -> m (Maybe Domain.Types.FeedbackBadge.FeedbackBadge))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FeedbackBadge.FeedbackBadge -> m ())
updateByPrimaryKey (Domain.Types.FeedbackBadge.FeedbackBadge {..}) = do {_now <- getCurrentTime;
                                                                         updateWithKV [Se.Set Beam.badge badge,
                                                                                       Se.Set Beam.badgeCount badgeCount,
                                                                                       Se.Set Beam.badgeKey badgeKey,
                                                                                       Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
                                                                                       Se.Set Beam.updatedAt _now,
                                                                                       Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                       Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



