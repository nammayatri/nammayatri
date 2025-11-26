{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FeedbackBadge where

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

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FeedbackBadge.FeedbackBadge -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FeedbackBadge.FeedbackBadge] -> m ())
createMany = traverse_ create

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.FeedbackBadge.FeedbackBadge]))
findByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FeedbackBadge.FeedbackBadge -> m (Maybe Domain.Types.FeedbackBadge.FeedbackBadge))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FeedbackBadge.FeedbackBadge -> m ())
updateByPrimaryKey (Domain.Types.FeedbackBadge.FeedbackBadge {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.badge badge,
      Se.Set Beam.badgeCount badgeCount,
      Se.Set Beam.badgeKey badgeKey,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FeedbackBadge Domain.Types.FeedbackBadge.FeedbackBadge where
  fromTType' (Beam.FeedbackBadgeT {..}) = do
    pure $
      Just
        Domain.Types.FeedbackBadge.FeedbackBadge
          { badge = badge,
            badgeCount = badgeCount,
            badgeKey = badgeKey,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.FeedbackBadge Domain.Types.FeedbackBadge.FeedbackBadge where
  toTType' (Domain.Types.FeedbackBadge.FeedbackBadge {..}) = do
    Beam.FeedbackBadgeT
      { Beam.badge = badge,
        Beam.badgeCount = badgeCount,
        Beam.badgeKey = badgeKey,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
