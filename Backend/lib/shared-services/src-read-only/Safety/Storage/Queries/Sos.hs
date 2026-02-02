{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.Storage.Queries.Sos where

import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Safety.Domain.Types.Common
import qualified Safety.Domain.Types.Sos
import qualified Safety.Storage.Beam.Sos as Beam
import qualified Safety.Storage.BeamFlow
import qualified Sequelize as Se

create :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Safety.Domain.Types.Sos.Sos -> m ())
create = createWithKV

createMany :: (Safety.Storage.BeamFlow.BeamFlow m r) => ([Safety.Domain.Types.Sos.Sos] -> m ())
createMany = traverse_ create

findById :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos -> m (Maybe Safety.Domain.Types.Sos.Sos))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPersonId :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Safety.Domain.Types.Common.Person -> m [Safety.Domain.Types.Sos.Sos])
findByPersonId personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByRideId :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Common.Ride) -> m (Maybe Safety.Domain.Types.Sos.Sos))
findByRideId rideId = do findOneWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId <$> rideId)]

findByTicketId :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Safety.Domain.Types.Sos.Sos))
findByTicketId ticketId = do findOneWithKV [Se.Is Beam.ticketId $ Se.Eq ticketId]

updateEntityType :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Safety.Domain.Types.Sos.SosEntityType -> Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos -> m ())
updateEntityType entityType id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.entityType entityType, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateMediaFiles :: (Safety.Storage.BeamFlow.BeamFlow m r) => ([Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile] -> Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos -> m ())
updateMediaFiles mediaFiles id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.mediaFiles (Kernel.Prelude.Just (Kernel.Types.Id.getId <$> mediaFiles)), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateState :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Safety.Domain.Types.Sos.SosState -> Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos -> m ())
updateState sosState id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.sosState sosState, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Safety.Domain.Types.Sos.SosStatus -> Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTrackingExpiresAt :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos -> m ())
updateTrackingExpiresAt trackingExpiresAt id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.trackingExpiresAt trackingExpiresAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos -> m (Maybe Safety.Domain.Types.Sos.Sos))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Safety.Domain.Types.Sos.Sos -> m ())
updateByPrimaryKey (Safety.Domain.Types.Sos.Sos {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.entityType entityType,
      Se.Set Beam.flow flow,
      Se.Set Beam.mediaFiles (Kernel.Prelude.Just (Kernel.Types.Id.getId <$> mediaFiles)),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.rideId (Kernel.Types.Id.getId <$> rideId),
      Se.Set Beam.sosState sosState,
      Se.Set Beam.status status,
      Se.Set Beam.ticketId ticketId,
      Se.Set Beam.trackingExpiresAt trackingExpiresAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Sos Safety.Domain.Types.Sos.Sos where
  fromTType' (Beam.SosT {..}) = do
    pure $
      Just
        Safety.Domain.Types.Sos.Sos
          { createdAt = createdAt,
            entityType = entityType,
            flow = flow,
            id = Kernel.Types.Id.Id id,
            mediaFiles = Kernel.Types.Id.Id <$> Kernel.Prelude.fromMaybe [] mediaFiles,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            rideId = Kernel.Types.Id.Id <$> rideId,
            sosState = sosState,
            status = status,
            ticketId = ticketId,
            trackingExpiresAt = trackingExpiresAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Sos Safety.Domain.Types.Sos.Sos where
  toTType' (Safety.Domain.Types.Sos.Sos {..}) = do
    Beam.SosT
      { Beam.createdAt = createdAt,
        Beam.entityType = entityType,
        Beam.flow = flow,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.mediaFiles = Kernel.Prelude.Just (Kernel.Types.Id.getId <$> mediaFiles),
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.rideId = Kernel.Types.Id.getId <$> rideId,
        Beam.sosState = sosState,
        Beam.status = status,
        Beam.ticketId = ticketId,
        Beam.trackingExpiresAt = trackingExpiresAt,
        Beam.updatedAt = updatedAt
      }
