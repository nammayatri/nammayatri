module Storage.Queries.AadhaarCardExtra where

import qualified Domain.Types.AadhaarCard as Domain
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import Sequelize as Se
import Storage.Beam.AadhaarCard as Beam
import Storage.Queries.OrphanInstances.AadhaarCard ()

findByPhoneNumberAndUpdate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe DbHash ->
  Kernel.Types.Documents.VerificationStatus ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  m ()
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

findByAadhaarNumberHash :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe DbHash -> m (Maybe Domain.AadhaarCard)
findByAadhaarNumberHash aadhaarNumberHash = do
  findOneWithKV [Se.Is Beam.aadhaarNumberHash $ Se.Eq aadhaarNumberHash]

findAllByEncryptedAadhaarNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe DbHash -> m [Domain.AadhaarCard]
findAllByEncryptedAadhaarNumber mbAadhaarNumberHash = do
  findAllWithKV
    [Se.Is Beam.aadhaarNumberHash $ Se.Eq mbAadhaarNumberHash]

upsertAadhaarRecord :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.AadhaarCard -> m ()
upsertAadhaarRecord a@Domain.AadhaarCard {..} =
  findOneWithKV [Se.Is Beam.driverId $ Se.Eq driverId.getId] >>= \case
    Just _ ->
      updateOneWithKV
        [ Se.Set Beam.consentTimestamp consentTimestamp,
          Se.Set Beam.dateOfBirth dateOfBirth,
          Se.Set Beam.nameOnCard nameOnCard,
          Se.Set Beam.aadhaarBackImageId $ aadhaarBackImageId <&> (.getId),
          Se.Set Beam.aadhaarFrontImageId $ aadhaarFrontImageId <&> (.getId),
          Se.Set Beam.maskedAadhaarNumber maskedAadhaarNumber,
          Se.Set Beam.address address,
          Se.Set Beam.updatedAt updatedAt,
          Se.Set Beam.verificationStatus verificationStatus,
          Se.Set Beam.aadhaarNumberHash (aadhaarNumber <&> (.hash)),
          Se.Set Beam.aadhaarNumberEncrypted (aadhaarNumber <&> unEncrypted . (.encrypted)),
          Se.Set Beam.driverGender driverGender,
          Se.Set Beam.driverImage driverImage,
          Se.Set Beam.driverImagePath driverImagePath
        ]
        [Se.Is Beam.driverId $ Se.Eq driverId.getId]
    Nothing -> createWithKV a
