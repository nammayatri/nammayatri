{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AlertIncidentTimeline where

import qualified Domain.Types.AlertIncident
import qualified Domain.Types.AlertIncidentTimeline
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AlertIncidentTimeline as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline] -> m ())
createMany = traverse_ create

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => m ()
create = do createWithKV []

findByIncidentId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.AlertIncident.AlertIncident -> m ([Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline]))
findByIncidentId limit offset incidentId = do findAllWithOptionsKV [Se.Is Beam.incidentId $ Se.Eq (Kernel.Types.Id.getId incidentId)] (Se.Asc Beam.eventTime) limit offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline -> m (Maybe Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline -> m ())
updateByPrimaryKey (Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.attachments attachments,
      Se.Set Beam.content content,
      Se.Set Beam.eventTime eventTime,
      Se.Set Beam.eventType eventType,
      Se.Set Beam.incidentId (Kernel.Types.Id.getId incidentId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.AlertIncidentTimeline Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline where
  fromTType' (Beam.AlertIncidentTimelineT {..}) = do
    pure $
      Just
        Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline
          { attachments = attachments,
            content = content,
            createdAt = createdAt,
            eventTime = eventTime,
            eventType = eventType,
            id = Kernel.Types.Id.Id id,
            incidentId = Kernel.Types.Id.Id incidentId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AlertIncidentTimeline Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline where
  toTType' (Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline {..}) = do
    Beam.AlertIncidentTimelineT
      { Beam.attachments = attachments,
        Beam.content = content,
        Beam.createdAt = createdAt,
        Beam.eventTime = eventTime,
        Beam.eventType = eventType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.incidentId = Kernel.Types.Id.getId incidentId,
        Beam.updatedAt = updatedAt
      }
