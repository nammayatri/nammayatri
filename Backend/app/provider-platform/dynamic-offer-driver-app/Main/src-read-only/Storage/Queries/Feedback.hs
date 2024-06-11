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
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.rideId (Kernel.Types.Id.getId rideId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Feedback Domain.Types.Feedback.Feedback where
  fromTType' (Beam.FeedbackT {..}) = do
    pure $
      Just
        Domain.Types.Feedback.Feedback
          { badge = badge,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            rideId = Kernel.Types.Id.Id rideId
          }

instance ToTType' Beam.Feedback Domain.Types.Feedback.Feedback where
  toTType' (Domain.Types.Feedback.Feedback {..}) = do
    Beam.FeedbackT
      { Beam.badge = badge,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rideId = Kernel.Types.Id.getId rideId
      }

{-
	DSL Source Link: file://./../../../spec/Storage/FeedBack.yaml
-}
