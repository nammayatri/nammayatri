{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverGstin where

import qualified Domain.Types.DriverGstin
import qualified Domain.Types.Image
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverGstin as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGstin.DriverGstin -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverGstin.DriverGstin] -> m ())
createMany = traverse_ create

deleteByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverGstin.DriverGstin))
findByDriverId driverId = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverGstin.DriverGstin -> m (Maybe Domain.Types.DriverGstin.DriverGstin))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.DriverGstin.DriverGstin))
findByImageId documentImageId1 = do findOneWithKV [Se.Is Beam.documentImageId1 $ Se.Eq (Kernel.Types.Id.getId documentImageId1)]

updateVerificationStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateVerificationStatus verificationStatus driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateVerificationStatusByImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatusByImageId verificationStatus documentImageId1 = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.documentImageId1 $ Se.Eq (Kernel.Types.Id.getId documentImageId1)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverGstin.DriverGstin -> m (Maybe Domain.Types.DriverGstin.DriverGstin))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGstin.DriverGstin -> m ())
updateByPrimaryKey (Domain.Types.DriverGstin.DriverGstin {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.constitutionOfBusiness constitutionOfBusiness,
      Se.Set Beam.dateOfLiability dateOfLiability,
      Se.Set Beam.documentImageId1 (Kernel.Types.Id.getId documentImageId1),
      Se.Set Beam.documentImageId2 (Kernel.Types.Id.getId <$> documentImageId2),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.driverName driverName,
      Se.Set Beam.gstinEncrypted (((gstin & unEncrypted . encrypted))),
      Se.Set Beam.gstinHash ((gstin & hash)),
      Se.Set Beam.isProvisional isProvisional,
      Se.Set Beam.legalName legalName,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.tradeName tradeName,
      Se.Set Beam.typeOfRegistration typeOfRegistration,
      Se.Set Beam.validFrom validFrom,
      Se.Set Beam.validUpto validUpto,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.verifiedBy verifiedBy,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverGstin Domain.Types.DriverGstin.DriverGstin where
  fromTType' (Beam.DriverGstinT {..}) = do
    pure $
      Just
        Domain.Types.DriverGstin.DriverGstin
          { address = address,
            constitutionOfBusiness = constitutionOfBusiness,
            dateOfLiability = dateOfLiability,
            documentImageId1 = Kernel.Types.Id.Id documentImageId1,
            documentImageId2 = Kernel.Types.Id.Id <$> documentImageId2,
            driverId = Kernel.Types.Id.Id driverId,
            driverName = driverName,
            gstin = EncryptedHashed (Encrypted gstinEncrypted) gstinHash,
            id = Kernel.Types.Id.Id id,
            isProvisional = isProvisional,
            legalName = legalName,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            tradeName = tradeName,
            typeOfRegistration = typeOfRegistration,
            validFrom = validFrom,
            validUpto = validUpto,
            verificationStatus = verificationStatus,
            verifiedBy = verifiedBy,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverGstin Domain.Types.DriverGstin.DriverGstin where
  toTType' (Domain.Types.DriverGstin.DriverGstin {..}) = do
    Beam.DriverGstinT
      { Beam.address = address,
        Beam.constitutionOfBusiness = constitutionOfBusiness,
        Beam.dateOfLiability = dateOfLiability,
        Beam.documentImageId1 = Kernel.Types.Id.getId documentImageId1,
        Beam.documentImageId2 = Kernel.Types.Id.getId <$> documentImageId2,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverName = driverName,
        Beam.gstinEncrypted = ((gstin & unEncrypted . encrypted)),
        Beam.gstinHash = (gstin & hash),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isProvisional = isProvisional,
        Beam.legalName = legalName,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.tradeName = tradeName,
        Beam.typeOfRegistration = typeOfRegistration,
        Beam.validFrom = validFrom,
        Beam.validUpto = validUpto,
        Beam.verificationStatus = verificationStatus,
        Beam.verifiedBy = verifiedBy,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
