{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RideEvidence where

import qualified Domain.Types.Ride
import qualified Domain.Types.RideEvidence
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RideEvidence as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideEvidence.RideEvidence -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RideEvidence.RideEvidence] -> m ())
createMany = traverse_ create

findAllByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ([Domain.Types.RideEvidence.RideEvidence]))
findAllByRideId rideId = do findAllWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RideEvidence.RideEvidence -> m (Maybe Domain.Types.RideEvidence.RideEvidence))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMediaFileId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> m (Maybe Domain.Types.RideEvidence.RideEvidence))
findByMediaFileId mediaFileId = do findOneWithKV [Se.Is Beam.mediaFileId $ Se.Eq (Kernel.Types.Id.getId mediaFileId)]

updateVisibility ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.RideEvidence.RideEvidence -> m ())
updateVisibility hiddenAt hiddenBy hiddenReason id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.hiddenAt hiddenAt, Se.Set Beam.hiddenBy hiddenBy, Se.Set Beam.hiddenReason hiddenReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RideEvidence.RideEvidence -> m (Maybe Domain.Types.RideEvidence.RideEvidence))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideEvidence.RideEvidence -> m ())
updateByPrimaryKey (Domain.Types.RideEvidence.RideEvidence {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.hiddenAt hiddenAt,
      Se.Set Beam.hiddenBy hiddenBy,
      Se.Set Beam.hiddenReason hiddenReason,
      Se.Set Beam.mediaFileId (Kernel.Types.Id.getId mediaFileId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
      Se.Set Beam.rideStatus rideStatus,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.RideEvidence Domain.Types.RideEvidence.RideEvidence where
  fromTType' (Beam.RideEvidenceT {..}) = do
    pure $
      Just
        Domain.Types.RideEvidence.RideEvidence
          { description = description,
            driverId = Kernel.Types.Id.Id driverId,
            hiddenAt = hiddenAt,
            hiddenBy = hiddenBy,
            hiddenReason = hiddenReason,
            id = Kernel.Types.Id.Id id,
            mediaFileId = Kernel.Types.Id.Id mediaFileId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rideId = Kernel.Types.Id.Id rideId,
            rideStatus = rideStatus,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RideEvidence Domain.Types.RideEvidence.RideEvidence where
  toTType' (Domain.Types.RideEvidence.RideEvidence {..}) = do
    Beam.RideEvidenceT
      { Beam.description = description,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.hiddenAt = hiddenAt,
        Beam.hiddenBy = hiddenBy,
        Beam.hiddenReason = hiddenReason,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.mediaFileId = Kernel.Types.Id.getId mediaFileId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.rideStatus = rideStatus,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
