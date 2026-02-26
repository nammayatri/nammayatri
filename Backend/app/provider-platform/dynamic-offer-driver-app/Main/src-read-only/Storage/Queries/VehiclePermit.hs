{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehiclePermit where

import qualified Domain.Types.Image
import qualified Domain.Types.Person
import qualified Domain.Types.VehiclePermit
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehiclePermit as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehiclePermit.VehiclePermit -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehiclePermit.VehiclePermit] -> m ())
createMany = traverse_ create

findByImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.VehiclePermit.VehiclePermit))
findByImageId documentImageId = do findOneWithKV [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId documentImageId)]

findByRcId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m [Domain.Types.VehiclePermit.VehiclePermit])
findByRcId rcId = do findAllWithKV [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId)]

findByRcIdAndDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.VehiclePermit.VehiclePermit])
findByRcIdAndDriverId rcId driverId = do findAllWithKV [Se.And [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId), Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateVerificationStatusByImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatusByImageId verificationStatus documentImageId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId documentImageId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehiclePermit.VehiclePermit -> m (Maybe Domain.Types.VehiclePermit.VehiclePermit))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehiclePermit.VehiclePermit -> m ())
updateByPrimaryKey (Domain.Types.VehiclePermit.VehiclePermit {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.documentImageId (Kernel.Types.Id.getId documentImageId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.issueDate issueDate,
      Se.Set Beam.nameOfPermitHolder nameOfPermitHolder,
      Se.Set Beam.permitExpiry permitExpiry,
      Se.Set Beam.permitNumberEncrypted (permitNumber & unEncrypted . encrypted),
      Se.Set Beam.permitNumberHash (permitNumber & hash),
      Se.Set Beam.purposeOfJourney purposeOfJourney,
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.regionCovered regionCovered,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VehiclePermit Domain.Types.VehiclePermit.VehiclePermit where
  fromTType' (Beam.VehiclePermitT {..}) = do
    pure $
      Just
        Domain.Types.VehiclePermit.VehiclePermit
          { documentImageId = Kernel.Types.Id.Id documentImageId,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            issueDate = issueDate,
            nameOfPermitHolder = nameOfPermitHolder,
            permitExpiry = permitExpiry,
            permitNumber = EncryptedHashed (Encrypted permitNumberEncrypted) permitNumberHash,
            purposeOfJourney = purposeOfJourney,
            rcId = Kernel.Types.Id.Id rcId,
            regionCovered = regionCovered,
            verificationStatus = verificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehiclePermit Domain.Types.VehiclePermit.VehiclePermit where
  toTType' (Domain.Types.VehiclePermit.VehiclePermit {..}) = do
    Beam.VehiclePermitT
      { Beam.documentImageId = Kernel.Types.Id.getId documentImageId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.issueDate = issueDate,
        Beam.nameOfPermitHolder = nameOfPermitHolder,
        Beam.permitExpiry = permitExpiry,
        Beam.permitNumberEncrypted = permitNumber & unEncrypted . encrypted,
        Beam.permitNumberHash = permitNumber & hash,
        Beam.purposeOfJourney = purposeOfJourney,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.regionCovered = regionCovered,
        Beam.verificationStatus = verificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
