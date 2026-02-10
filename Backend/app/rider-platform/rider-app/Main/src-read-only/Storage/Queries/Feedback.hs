{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Feedback where

import qualified Domain.Types.Feedback
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Feedback as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Feedback.Feedback -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Feedback.Feedback] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Feedback.Feedback -> m (Maybe Domain.Types.Feedback.Feedback))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Feedback.Feedback -> m ())
updateByPrimaryKey (Domain.Types.Feedback.Feedback {..}) = do
  updateWithKV
    [ Se.Set Beam.badge badge,
      Se.Set Beam.badgeKey badgeKey,
      Se.Set Beam.rating rating,
      Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Feedback Domain.Types.Feedback.Feedback where
  fromTType' (Beam.FeedbackT {..}) = do
    pure $
      Just
        Domain.Types.Feedback.Feedback
          { badge = badge,
            badgeKey = badgeKey,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            rating = rating,
            rideId = Kernel.Types.Id.Id rideId,
            riderId = Kernel.Types.Id.Id riderId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.Feedback Domain.Types.Feedback.Feedback where
  toTType' (Domain.Types.Feedback.Feedback {..}) = do
    Beam.FeedbackT
      { Beam.badge = badge,
        Beam.badgeKey = badgeKey,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rating = rating,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
