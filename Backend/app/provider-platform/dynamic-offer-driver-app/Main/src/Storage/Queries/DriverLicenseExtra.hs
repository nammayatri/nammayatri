module Storage.Queries.DriverLicenseExtra where

import Domain.Types.DriverLicense
import Domain.Types.Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime, throwError)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverLicense as BeamDL
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.OrphanInstances.DriverLicense ()
import Storage.Queries.OrphanInstances.Person ()

-- Extra code goes here --
upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DriverLicense -> m ()
upsert a@DriverLicense {..} = do
  mbExistingByDriver <- findOneWithKV [Se.Is BeamDL.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
  let dlHash = a.licenseNumber & (.hash)
  mbDirectByNumber <-
    findOneWithKV
      [ Se.And
          [ Se.Is BeamDL.licenseNumberHash $ Se.Eq dlHash,
            Se.Is BeamDL.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId)
          ]
      ]
  mbExistingByNumber <- case (mbDirectByNumber, merchantId) of
    (Just _, _) -> pure mbDirectByNumber
    (Nothing, Just mId) -> resolveLegacyByHash dlHash mId
    (Nothing, Nothing) -> pure Nothing
  whenJust mbExistingByNumber $ \existingLicense ->
    when (existingLicense.driverId /= driverId) $
      throwError $ InternalError $ "Driver ID mismatch for license: existing driver is " <> existingLicense.driverId.getId <> " but trying to update with " <> driverId.getId
  case mbExistingByDriver of
    Just existingLicense ->
      updateOneWithKV
        [ Se.Set BeamDL.driverDob driverDob,
          Se.Set BeamDL.driverName driverName,
          Se.Set BeamDL.documentImageId1 documentImageId1.getId,
          Se.Set BeamDL.documentImageId2 (Kernel.Types.Id.getId <$> documentImageId2),
          Se.Set BeamDL.licenseNumberEncrypted (licenseNumber & unEncrypted . encrypted),
          Se.Set BeamDL.licenseNumberHash (licenseNumber & hash),
          Se.Set BeamDL.rejectReason rejectReason,
          Se.Set BeamDL.licenseExpiry licenseExpiry,
          Se.Set BeamDL.classOfVehicles classOfVehicles,
          Se.Set BeamDL.driverId (Kernel.Types.Id.getId driverId),
          Se.Set BeamDL.merchantId (Kernel.Types.Id.getId <$> merchantId),
          Se.Set BeamDL.verificationStatus verificationStatus,
          Se.Set BeamDL.failedRules failedRules,
          Se.Set BeamDL.updatedAt updatedAt
        ]
        [Se.Is BeamDL.id $ Se.Eq existingLicense.id.getId]
    Nothing -> createWithKV a

deleteByDriverIdAndStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> VerificationStatus -> m ()
deleteByDriverIdAndStatus driverId status =
  deleteWithKV [Se.And [Se.Is BeamDL.driverId $ Se.Eq driverId.getId, Se.Is BeamDL.verificationStatus $ Se.Eq status]]

findByDLNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> Id DM.Merchant -> m (Maybe DriverLicense)
findByDLNumber dlNumber merchantId = do
  dlNumberHash <- getDbHash dlNumber
  mbDirect <-
    findOneWithKV
      [ Se.And
          [ Se.Is BeamDL.licenseNumberHash $ Se.Eq dlNumberHash,
            Se.Is BeamDL.merchantId $ Se.Eq (Just merchantId.getId)
          ]
      ]
  case mbDirect of
    Just _ -> pure mbDirect
    Nothing -> resolveLegacyByHash dlNumberHash merchantId

resolveLegacyByHash ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DbHash ->
  Id DM.Merchant ->
  m (Maybe DriverLicense)
resolveLegacyByHash dlNumberHash merchantId = do
  mbLegacy <-
    findOneWithKV
      [ Se.And
          [ Se.Is BeamDL.licenseNumberHash $ Se.Eq dlNumberHash,
            Se.Is BeamDL.merchantId $ Se.Eq Nothing
          ]
      ]
  case mbLegacy of
    Nothing -> pure Nothing
    Just dl -> do
      mbPerson :: Maybe DP.Person <- findOneWithKV [Se.Is BeamP.id $ Se.Eq dl.driverId.getId]
      case (.merchantId) <$> mbPerson of
        Nothing -> pure Nothing
        Just personMerchantId -> do
          updateOneWithKV
            [Se.Set BeamDL.merchantId (Just personMerchantId.getId)]
            [Se.Is BeamDL.id $ Se.Eq dl.id.getId]
          pure $
            if personMerchantId == merchantId
              then Just (dl {merchantId = Just personMerchantId} :: DriverLicense)
              else Nothing

findByDLNumberAndStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> VerificationStatus -> Id DM.Merchant -> m (Maybe DriverLicense)
findByDLNumberAndStatus dlNumber verificationStatus merchantId = do
  dlNumberHash <- getDbHash dlNumber
  mbDirect <-
    findOneWithKV
      [ Se.And
          [ Se.Is BeamDL.verificationStatus $ Se.Eq verificationStatus,
            Se.Is BeamDL.licenseNumberHash $ Se.Eq dlNumberHash,
            Se.Is BeamDL.merchantId $ Se.Eq (Just merchantId.getId)
          ]
      ]
  case mbDirect of
    Just _ -> pure mbDirect
    Nothing -> do
      mbLegacy <- resolveLegacyByHash dlNumberHash merchantId
      pure $ mbLegacy >>= \dl -> if dl.verificationStatus == verificationStatus then Just dl else Nothing

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

-- | Point the licence at it's other approved versionImages(these images are already VALID) when current is rejected and sets the row VALID.
updateDocImagesAndMarkValidById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DriverLicense ->
  Id Image ->
  Maybe (Id Image) ->
  m ()
updateDocImagesAndMarkValidById (Id dlId) (Id imageId) mbImageId2 = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDL.documentImageId1 imageId,
      Se.Set BeamDL.documentImageId2 (getId <$> mbImageId2),
      Se.Set BeamDL.verificationStatus VALID,
      Se.Set BeamDL.rejectReason Nothing,
      Se.Set BeamDL.updatedAt _now
    ]
    [Se.Is BeamDL.id $ Se.Eq dlId]
