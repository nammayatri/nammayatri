{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleFitnessCertificate where

import qualified Domain.Types.Image
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleFitnessCertificate
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleFitnessCertificate as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate] -> m ())
createMany = traverse_ create

findByImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate))
findByImageId documentImageId = do findOneWithKV [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId documentImageId)]

findByRcIdAndDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate])
findByRcIdAndDriverId rcId driverId = do findAllWithKV [Se.And [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId), Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateVerificationStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatus verificationStatus documentImageId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId documentImageId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate -> m (Maybe Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate -> m ())
updateByPrimaryKey (Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.applicationNumberEncrypted (applicationNumber & unEncrypted . encrypted),
      Se.Set Beam.applicationNumberHash (applicationNumber & hash),
      Se.Set Beam.categoryOfVehicle categoryOfVehicle,
      Se.Set Beam.documentImageId (Kernel.Types.Id.getId documentImageId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.fitnessExpiry fitnessExpiry,
      Se.Set Beam.inspectingAuthority inspectingAuthority,
      Se.Set Beam.inspectingOn inspectingOn,
      Se.Set Beam.nextInspectionDate nextInspectionDate,
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.receiptDate receiptDate,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VehicleFitnessCertificate Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate where
  fromTType' (Beam.VehicleFitnessCertificateT {..}) = do
    pure $
      Just
        Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate
          { applicationNumber = EncryptedHashed (Encrypted applicationNumberEncrypted) applicationNumberHash,
            categoryOfVehicle = categoryOfVehicle,
            documentImageId = Kernel.Types.Id.Id documentImageId,
            driverId = Kernel.Types.Id.Id driverId,
            fitnessExpiry = fitnessExpiry,
            id = Kernel.Types.Id.Id id,
            inspectingAuthority = inspectingAuthority,
            inspectingOn = inspectingOn,
            nextInspectionDate = nextInspectionDate,
            rcId = Kernel.Types.Id.Id rcId,
            receiptDate = receiptDate,
            verificationStatus = verificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleFitnessCertificate Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate where
  toTType' (Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate {..}) = do
    Beam.VehicleFitnessCertificateT
      { Beam.applicationNumberEncrypted = applicationNumber & unEncrypted . encrypted,
        Beam.applicationNumberHash = applicationNumber & hash,
        Beam.categoryOfVehicle = categoryOfVehicle,
        Beam.documentImageId = Kernel.Types.Id.getId documentImageId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.fitnessExpiry = fitnessExpiry,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.inspectingAuthority = inspectingAuthority,
        Beam.inspectingOn = inspectingOn,
        Beam.nextInspectionDate = nextInspectionDate,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.receiptDate = receiptDate,
        Beam.verificationStatus = verificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
