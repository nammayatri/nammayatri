{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CallFeedback where

import qualified Domain.Types.CallFeedback
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CallFeedback as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CallFeedback.CallFeedback -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CallFeedback.CallFeedback] -> m ())
createMany = traverse_ create

findByCallId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.CallFeedback.CallFeedback))
findByCallId callId = do findOneWithKV [Se.Is Beam.callId $ Se.Eq callId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CallFeedback.CallFeedback -> m (Maybe Domain.Types.CallFeedback.CallFeedback))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.CallFeedback.CallFeedback -> m (Maybe Domain.Types.CallFeedback.CallFeedback))
findByPrimaryKey callId id = do findOneWithKV [Se.And [Se.Is Beam.callId $ Se.Eq callId, Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CallFeedback.CallFeedback -> m ())
updateByPrimaryKey (Domain.Types.CallFeedback.CallFeedback {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.entityId entityId,
      Se.Set Beam.optionIds optionIds,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.callId $ Se.Eq callId, Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CallFeedback Domain.Types.CallFeedback.CallFeedback where
  fromTType' (Beam.CallFeedbackT {..}) = do
    pure $
      Just
        Domain.Types.CallFeedback.CallFeedback
          { callId = callId,
            entityId = entityId,
            id = Kernel.Types.Id.Id id,
            optionIds = optionIds,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CallFeedback Domain.Types.CallFeedback.CallFeedback where
  toTType' (Domain.Types.CallFeedback.CallFeedback {..}) = do
    Beam.CallFeedbackT
      { Beam.callId = callId,
        Beam.entityId = entityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.optionIds = optionIds,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
