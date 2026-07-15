module Storage.Queries.BusinessLicenseExtra where

import Domain.Types.BusinessLicense
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.BusinessLicense as Beam
import Storage.Queries.BusinessLicense ()

-- Extra code goes here --

findAllByLicenseNumberHash :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> m [BusinessLicense]
findAllByLicenseNumberHash licenseNumberHash =
  findAllWithKV [Se.Is Beam.licenseNumberHash $ Se.Eq licenseNumberHash]

findByLicenseNumberHashAndStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> Documents.VerificationStatus -> m (Maybe BusinessLicense)
findByLicenseNumberHashAndStatus licenseNumberHash status =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.licenseNumberHash $ Se.Eq licenseNumberHash,
          Se.Is Beam.verificationStatus $ Se.Eq status
        ]
    ]

findByPersonIdAndVerificationStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Documents.VerificationStatus -> m (Maybe BusinessLicense)
findByPersonIdAndVerificationStatus driverId status =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq driverId.getId,
          Se.Is Beam.verificationStatus $ Se.Eq status
        ]
    ]

deleteByDriverIdAndStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Documents.VerificationStatus -> m ()
deleteByDriverIdAndStatus driverId status =
  deleteWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq driverId.getId, Se.Is Beam.verificationStatus $ Se.Eq status]]

upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => BusinessLicense -> m ()
upsert a@BusinessLicense {..} =
  findOneWithKV [Se.Is Beam.driverId $ Se.Eq driverId.getId] >>= \case
    Just _ ->
      updateOneWithKV
        [ Se.Set Beam.documentImageId documentImageId.getId,
          Se.Set Beam.licenseNumberEncrypted (licenseNumber & unEncrypted . encrypted),
          Se.Set Beam.licenseNumberHash (licenseNumber & hash),
          Se.Set Beam.licenseExpiry licenseExpiry,
          Se.Set Beam.verificationStatus verificationStatus,
          Se.Set Beam.updatedAt updatedAt
        ]
        [Se.Is Beam.driverId $ Se.Eq driverId.getId]
    Nothing -> createWithKV a
