{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverUdyam where

import qualified Domain.Types.DriverUdyam
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverUdyam as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverUdyam.DriverUdyam -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverUdyam.DriverUdyam] -> m ())
createMany = traverse_ create

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverUdyam.DriverUdyam))
findByDriverId driverId = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverUdyam.DriverUdyam -> m (Maybe Domain.Types.DriverUdyam.DriverUdyam))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateVerificationStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateVerificationStatus verificationStatus driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverUdyam.DriverUdyam -> m (Maybe Domain.Types.DriverUdyam.DriverUdyam))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverUdyam.DriverUdyam -> m ())
updateByPrimaryKey (Domain.Types.DriverUdyam.DriverUdyam {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.enterpriseName enterpriseName,
      Se.Set Beam.enterpriseType enterpriseType,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.udyamNumberEncrypted (((udyamNumber & unEncrypted . encrypted))),
      Se.Set Beam.udyamNumberHash ((udyamNumber & hash)),
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.verifiedBy verifiedBy,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverUdyam Domain.Types.DriverUdyam.DriverUdyam where
  fromTType' (Beam.DriverUdyamT {..}) = do
    pure $
      Just
        Domain.Types.DriverUdyam.DriverUdyam
          { driverId = Kernel.Types.Id.Id driverId,
            enterpriseName = enterpriseName,
            enterpriseType = enterpriseType,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            udyamNumber = EncryptedHashed (Encrypted udyamNumberEncrypted) udyamNumberHash,
            verificationStatus = verificationStatus,
            verifiedBy = verifiedBy,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverUdyam Domain.Types.DriverUdyam.DriverUdyam where
  toTType' (Domain.Types.DriverUdyam.DriverUdyam {..}) = do
    Beam.DriverUdyamT
      { Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.enterpriseName = enterpriseName,
        Beam.enterpriseType = enterpriseType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.udyamNumberEncrypted = ((udyamNumber & unEncrypted . encrypted)),
        Beam.udyamNumberHash = (udyamNumber & hash),
        Beam.verificationStatus = verificationStatus,
        Beam.verifiedBy = verifiedBy,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
