{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AadhaarCard (module Storage.Queries.AadhaarCard, module ReExport) where

import qualified Domain.Types.AadhaarCard
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AadhaarCard as Beam
import Storage.Queries.AadhaarCardExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AadhaarCard.AadhaarCard -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AadhaarCard.AadhaarCard] -> m ())
createMany = traverse_ create

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByAadhaarNumberHash :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> m (Maybe Domain.Types.AadhaarCard.AadhaarCard))
findByAadhaarNumberHash aadhaarNumberHash = do findOneWithKV [Se.Is Beam.aadhaarNumberHash $ Se.Eq aadhaarNumberHash]

findByBackImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image) -> m (Maybe Domain.Types.AadhaarCard.AadhaarCard))
findByBackImageId aadhaarBackImageId = do findOneWithKV [Se.Is Beam.aadhaarBackImageId $ Se.Eq (Kernel.Types.Id.getId <$> aadhaarBackImageId)]

findByFrontImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image) -> m (Maybe Domain.Types.AadhaarCard.AadhaarCard))
findByFrontImageId aadhaarFrontImageId = do findOneWithKV [Se.Is Beam.aadhaarFrontImageId $ Se.Eq (Kernel.Types.Id.getId <$> aadhaarFrontImageId)]

findByPhoneNumberAndUpdate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
findByPhoneNumberAndUpdate nameOnCard driverGender dateOfBirth aadhaarNumberHash verificationStatus driverId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.nameOnCard nameOnCard,
      Se.Set Beam.driverGender driverGender,
      Se.Set Beam.dateOfBirth dateOfBirth,
      Se.Set Beam.aadhaarNumberHash aadhaarNumberHash,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateDriverImagePath :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDriverImagePath driverImagePath driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverImagePath driverImagePath, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateMerchantIdAndCityIdByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateMerchantIdAndCityIdByDriverId merchantId merchantOperatingCityId driverId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateVerificationStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateVerificationStatus verificationStatus driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.AadhaarCard.AadhaarCard))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AadhaarCard.AadhaarCard -> m ())
updateByPrimaryKey (Domain.Types.AadhaarCard.AadhaarCard {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.aadhaarBackImageId (Kernel.Types.Id.getId <$> aadhaarBackImageId),
      Se.Set Beam.aadhaarFrontImageId (Kernel.Types.Id.getId <$> aadhaarFrontImageId),
      Se.Set Beam.aadhaarNumberHash aadhaarNumberHash,
      Se.Set Beam.address address,
      Se.Set Beam.consent consent,
      Se.Set Beam.consentTimestamp consentTimestamp,
      Se.Set Beam.dateOfBirth dateOfBirth,
      Se.Set Beam.driverGender driverGender,
      Se.Set Beam.driverImage driverImage,
      Se.Set Beam.driverImagePath driverImagePath,
      Se.Set Beam.maskedAadhaarNumber maskedAadhaarNumber,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.nameOnCard nameOnCard,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.verificationStatus verificationStatus
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
