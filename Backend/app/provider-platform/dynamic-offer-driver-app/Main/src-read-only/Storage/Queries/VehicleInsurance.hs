{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleInsurance where

import qualified Domain.Types.IdfyVerification
import qualified Domain.Types.Image
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleInsurance
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleInsurance as Beam

create :: KvDbFlow m r => (Domain.Types.VehicleInsurance.VehicleInsurance -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.VehicleInsurance.VehicleInsurance] -> m ())
createMany = traverse_ create

findByImageId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.VehicleInsurance.VehicleInsurance))
findByImageId (Kernel.Types.Id.Id documentImageId) = do findOneWithKV [Se.Is Beam.documentImageId $ Se.Eq documentImageId]

findByRcIdAndDriverId ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.VehicleInsurance.VehicleInsurance])
findByRcIdAndDriverId (Kernel.Types.Id.Id rcId) (Kernel.Types.Id.Id driverId) = do findAllWithKV [Se.And [Se.Is Beam.rcId $ Se.Eq rcId, Se.Is Beam.driverId $ Se.Eq driverId]]

updateVerificationStatus :: KvDbFlow m r => (Domain.Types.IdfyVerification.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatus verificationStatus (Kernel.Types.Id.Id documentImageId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.documentImageId $ Se.Eq documentImageId]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.VehicleInsurance.VehicleInsurance -> m (Maybe Domain.Types.VehicleInsurance.VehicleInsurance))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.VehicleInsurance.VehicleInsurance -> m ())
updateByPrimaryKey (Domain.Types.VehicleInsurance.VehicleInsurance {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.documentImageId (Kernel.Types.Id.getId documentImageId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.insuredName insuredName,
      Se.Set Beam.issueDate issueDate,
      Se.Set Beam.limitsOfLiability limitsOfLiability,
      Se.Set Beam.policyExpiry policyExpiry,
      Se.Set Beam.policyNumberEncrypted (policyNumber & unEncrypted . encrypted),
      Se.Set Beam.policyNumberHash (policyNumber & hash),
      Se.Set Beam.policyProvider policyProvider,
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VehicleInsurance Domain.Types.VehicleInsurance.VehicleInsurance where
  fromTType' (Beam.VehicleInsuranceT {..}) = do
    pure $
      Just
        Domain.Types.VehicleInsurance.VehicleInsurance
          { documentImageId = Kernel.Types.Id.Id documentImageId,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            insuredName = insuredName,
            issueDate = issueDate,
            limitsOfLiability = limitsOfLiability,
            policyExpiry = policyExpiry,
            policyNumber = EncryptedHashed (Encrypted policyNumberEncrypted) policyNumberHash,
            policyProvider = policyProvider,
            rcId = Kernel.Types.Id.Id rcId,
            verificationStatus = verificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleInsurance Domain.Types.VehicleInsurance.VehicleInsurance where
  toTType' (Domain.Types.VehicleInsurance.VehicleInsurance {..}) = do
    Beam.VehicleInsuranceT
      { Beam.documentImageId = Kernel.Types.Id.getId documentImageId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.insuredName = insuredName,
        Beam.issueDate = issueDate,
        Beam.limitsOfLiability = limitsOfLiability,
        Beam.policyExpiry = policyExpiry,
        Beam.policyNumberEncrypted = policyNumber & unEncrypted . encrypted,
        Beam.policyNumberHash = policyNumber & hash,
        Beam.policyProvider = policyProvider,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.verificationStatus = verificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
