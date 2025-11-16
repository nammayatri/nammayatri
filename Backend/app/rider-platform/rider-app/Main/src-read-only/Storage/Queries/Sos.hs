{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Sos where

import qualified Domain.Types.Ride
import qualified Domain.Types.Sos
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Sos as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Sos.Sos -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Sos.Sos] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Sos.Sos -> m (Maybe Domain.Types.Sos.Sos))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m (Maybe Domain.Types.Sos.Sos))
findByRideId rideId = do findOneWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)]

findByTicketId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.Sos.Sos))
findByTicketId ticketId = do findOneWithKV [Se.Is Beam.ticketId $ Se.Eq ticketId]

updateMediaFiles :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile] -> Kernel.Types.Id.Id Domain.Types.Sos.Sos -> m ())
updateMediaFiles mediaFiles id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.mediaFiles (Kernel.Prelude.Just (Kernel.Types.Id.getId <$> mediaFiles)), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Sos.SosStatus -> Kernel.Types.Id.Id Domain.Types.Sos.Sos -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Sos.Sos -> m (Maybe Domain.Types.Sos.Sos))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Sos.Sos -> m ())
updateByPrimaryKey (Domain.Types.Sos.Sos {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flow flow,
      Se.Set Beam.mediaFiles (Kernel.Prelude.Just (Kernel.Types.Id.getId <$> mediaFiles)),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
      Se.Set Beam.status status,
      Se.Set Beam.ticketId ticketId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Sos Domain.Types.Sos.Sos where
  fromTType' (Beam.SosT {..}) = do
    pure $
      Just
        Domain.Types.Sos.Sos
          { flow = flow,
            id = Kernel.Types.Id.Id id,
            mediaFiles = Kernel.Types.Id.Id <$> Kernel.Prelude.fromMaybe [] mediaFiles,
            personId = Kernel.Types.Id.Id personId,
            rideId = Kernel.Types.Id.Id rideId,
            status = status,
            ticketId = ticketId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Sos Domain.Types.Sos.Sos where
  toTType' (Domain.Types.Sos.Sos {..}) = do
    Beam.SosT
      { Beam.flow = flow,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.mediaFiles = Kernel.Prelude.Just (Kernel.Types.Id.getId <$> mediaFiles),
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.status = status,
        Beam.ticketId = ticketId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
