{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleInsurance (module Storage.Queries.VehicleInsurance, module ReExport) where

import qualified Domain.Types.Image
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleInsurance
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleInsurance as Beam
import Storage.Queries.VehicleInsuranceExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleInsurance.VehicleInsurance -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleInsurance.VehicleInsurance] -> m ())
createMany = traverse_ create

findByImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.VehicleInsurance.VehicleInsurance))
findByImageId documentImageId = do findOneWithKV [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId documentImageId)]

findByRcId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m [Domain.Types.VehicleInsurance.VehicleInsurance])
findByRcId rcId = do findAllWithKV [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId)]

findByRcIdAndDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.VehicleInsurance.VehicleInsurance]))
findByRcIdAndDriverId rcId driverId = do findAllWithKV [Se.And [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId), Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateVerificationStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatus verificationStatus documentImageId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId documentImageId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleInsurance.VehicleInsurance -> m (Maybe Domain.Types.VehicleInsurance.VehicleInsurance))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleInsurance.VehicleInsurance -> m ())
updateByPrimaryKey (Domain.Types.VehicleInsurance.VehicleInsurance {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.documentImageId (Kernel.Types.Id.getId documentImageId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.insuredName insuredName,
      Se.Set Beam.issueDate issueDate,
      Se.Set Beam.limitsOfLiability limitsOfLiability,
      Se.Set Beam.policyExpiry policyExpiry,
      Se.Set Beam.policyNumberEncrypted (((policyNumber & unEncrypted . encrypted))),
      Se.Set Beam.policyNumberHash ((policyNumber & hash)),
      Se.Set Beam.policyProvider policyProvider,
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.rejectReason rejectReason,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
