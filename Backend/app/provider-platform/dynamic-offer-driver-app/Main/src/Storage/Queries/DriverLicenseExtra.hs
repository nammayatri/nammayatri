module Storage.Queries.DriverLicenseExtra where

import Domain.Types.DriverLicense
import Domain.Types.Image
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime, throwError)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverLicense as BeamDL
import Storage.Queries.OrphanInstances.DriverLicense ()

-- Extra code goes here --
upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DriverLicense -> m ()
upsert a@DriverLicense {..} = do
  res <- findOneWithKV [Se.Is BeamDL.licenseNumberHash $ Se.Eq (a.licenseNumber & (.hash))]
  case res of
    Just existingLicense -> do
      when (existingLicense.driverId /= driverId) $ do
        throwError $ InternalError $ "Driver ID mismatch for license: existing driver is " <> existingLicense.driverId.getId <> " but trying to update with " <> driverId.getId
      updateOneWithKV
        [ Se.Set BeamDL.driverDob driverDob,
          Se.Set BeamDL.driverName driverName,
          Se.Set BeamDL.licenseExpiry licenseExpiry,
          Se.Set BeamDL.classOfVehicles classOfVehicles,
          Se.Set BeamDL.verificationStatus verificationStatus,
          Se.Set BeamDL.failedRules failedRules,
          Se.Set BeamDL.updatedAt updatedAt
        ]
        [Se.Is BeamDL.licenseNumberHash $ Se.Eq (a.licenseNumber & (.hash))]
    Nothing -> createWithKV a

findByDLNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> m (Maybe DriverLicense)
findByDLNumber dlNumber = do
  dlNumberHash <- getDbHash dlNumber
  findOneWithKV [Se.Is BeamDL.licenseNumberHash $ Se.Eq dlNumberHash]

findByImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.DriverLicense.DriverLicense))
findByImageId (Id imageId1) = findOneWithKV [Se.Or [Se.Is BeamDL.documentImageId1 $ Se.Eq imageId1, Se.Is BeamDL.documentImageId2 $ Se.Eq (Just imageId1)]]

findAllByImageId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Image] -> m [DriverLicense]
findAllByImageId imageIds = findAllWithKV [Se.Or [Se.Is BeamDL.documentImageId1 $ Se.In $ map (.getId) imageIds, Se.Is BeamDL.documentImageId2 $ Se.In $ map (Just . (.getId)) imageIds]]

updateVerificationStatusAndRejectReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Documents.VerificationStatus -> Text -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatusAndRejectReason verificationStatus rejectReason (Kernel.Types.Id.Id imageId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamDL.verificationStatus verificationStatus, Se.Set BeamDL.rejectReason (Just rejectReason), Se.Set BeamDL.updatedAt _now] [Se.Or [Se.Is BeamDL.documentImageId1 $ Se.Eq imageId, Se.Is BeamDL.documentImageId2 $ Se.Eq (Just imageId)]]

-- | Update documentImageId1, verificationStatus, and rejectReason keyed on
-- the DL's own id. Used in the reject path when the row's documentImageId1 is
-- stale (re-upload case) so the image pointer is re-pointed atomically.
updateDocImageAndStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DriverLicense ->
  Id Image ->
  VerificationStatus ->
  Text ->
  m ()
updateDocImageAndStatusById (Id dlId) (Id newImageId) status rejectReason = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDL.documentImageId1 newImageId,
      Se.Set BeamDL.verificationStatus status,
      Se.Set BeamDL.rejectReason (Just rejectReason),
      Se.Set BeamDL.updatedAt _now
    ]
    [Se.Is BeamDL.id $ Se.Eq dlId]
