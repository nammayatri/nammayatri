{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverLicense (module Storage.Queries.DriverLicense, module ReExport) where

import qualified Domain.Types.DriverLicense
import qualified Domain.Types.IdfyVerification
import qualified Domain.Types.Image
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverLicense as Beam
import Storage.Queries.DriverLicenseExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.DriverLicense.DriverLicense -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.DriverLicense.DriverLicense] -> m ())
createMany = traverse_ create

deleteByDriverId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId (Kernel.Types.Id.Id driverId) = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findByDriverId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverLicense.DriverLicense))
findByDriverId (Kernel.Types.Id.Id driverId) = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverLicense.DriverLicense -> m (Maybe Domain.Types.DriverLicense.DriverLicense))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

updateVerificationStatus :: KvDbFlow m r => (Domain.Types.IdfyVerification.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatus verificationStatus (Kernel.Types.Id.Id documentImageId1) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.documentImageId1 $ Se.Eq documentImageId1]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverLicense.DriverLicense -> m (Maybe Domain.Types.DriverLicense.DriverLicense))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.DriverLicense.DriverLicense -> m ())
updateByPrimaryKey (Domain.Types.DriverLicense.DriverLicense {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.classOfVehicles classOfVehicles,
      Se.Set Beam.consent consent,
      Se.Set Beam.consentTimestamp consentTimestamp,
      Se.Set Beam.documentImageId1 (Kernel.Types.Id.getId documentImageId1),
      Se.Set Beam.documentImageId2 (Kernel.Types.Id.getId <$> documentImageId2),
      Se.Set Beam.driverDob driverDob,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.driverName driverName,
      Se.Set Beam.failedRules failedRules,
      Se.Set Beam.licenseExpiry licenseExpiry,
      Se.Set Beam.licenseNumberEncrypted (licenseNumber & unEncrypted . encrypted),
      Se.Set Beam.licenseNumberHash (licenseNumber & hash),
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
