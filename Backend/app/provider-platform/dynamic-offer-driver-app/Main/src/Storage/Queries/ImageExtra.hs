module Storage.Queries.ImageExtra where

import qualified Data.List as DL
import qualified Data.Time as DT
import Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleRegistrationCertificate as DReg
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (on)
import Kernel.Types.Common
import qualified Kernel.Types.Documents
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, logWarning)
import Kernel.Utils.Error.Throwing
import qualified Sequelize as Se
import qualified Storage.Beam.Image as BeamI
import qualified Storage.Cac.TransporterConfig as QTC
import Storage.Queries.OrphanInstances.Image ()
import qualified Storage.Queries.Person as QP
import Tools.Error
import Utils.Common.Cac.KeyNameConstants

-- Extra code goes here --
findAllByPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DTC.TransporterConfig -> Id Person -> m [DImage.Image]
findAllByPersonId transporterConfig personId = do
  let onboardingDocsCountLimit = fromMaybe 50 transporterConfig.onboardingDocsCountLimit
  images <-
    findAllWithOptionsKV
      [ Se.And
          [Se.Is BeamI.personId $ Se.Eq $ getId personId]
      ]
      (Se.Desc BeamI.createdAt)
      (Just onboardingDocsCountLimit)
      (Just 0)
  let imagesCount = length images
  when (imagesCount >= onboardingDocsCountLimit) $ do
    logWarning $ "Onboarding docs count limit reached. Some docs probably would be skipped: driverId: " <> show personId <> "; count: " <> show imagesCount
  pure images

findRecentByRcIdAndImageTypes :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DTC.TransporterConfig -> Id DReg.VehicleRegistrationCertificate -> [DocumentType] -> m [DImage.Image]
findRecentByRcIdAndImageTypes transporterConfig rcId imageTypes = do
  let onboardingDocsCountLimit = fromMaybe 50 transporterConfig.onboardingDocsCountLimit
  let onboardingRetryTimeInHours = transporterConfig.onboardingRetryTimeInHours
  now <- getCurrentTime
  images <-
    findAllWithOptionsKV
      [ Se.And
          [ Se.Is BeamI.rcId $ Se.Eq $ Just rcId.getId,
            Se.Is BeamI.createdAt $ Se.GreaterThanOrEq (hoursAgo onboardingRetryTimeInHours now),
            Se.Is BeamI.imageType $ Se.In imageTypes
          ]
      ]
      (Se.Desc BeamI.createdAt)
      (Just onboardingDocsCountLimit)
      (Just 0)
  let imagesCount = length images
  when (imagesCount >= onboardingDocsCountLimit) $ do
    logWarning $ "Onboarding docs count limit reached. Some docs probably would be skipped: rcId: " <> show rcId <> "; count: " <> show imagesCount
  pure images

findRecentByPersonIdAndImageType :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> DocumentType -> m [DImage.Image]
findRecentByPersonIdAndImageType personId imgType = do
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- QTC.findByMerchantOpCityId person.merchantOperatingCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  let onboardingRetryTimeInHours = transporterConfig.onboardingRetryTimeInHours
  now <- getCurrentTime
  findAllWithOptionsKV
    [ Se.And
        [Se.Is BeamI.personId $ Se.Eq $ getId personId, Se.Is BeamI.imageType $ Se.Eq imgType, Se.Is BeamI.createdAt $ Se.GreaterThanOrEq (hoursAgo onboardingRetryTimeInHours now)]
    ]
    (Se.Desc BeamI.createdAt)
    Nothing
    Nothing

hoursAgo :: Int -> UTCTime -> UTCTime
hoursAgo i now = negate (intToNominalDiffTime $ 3600 * i) `DT.addUTCTime` now

findByPersonIdImageTypeAndValidationStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> DocumentType -> DImage.SelfieFetchStatus -> m (Maybe DImage.Image)
findByPersonIdImageTypeAndValidationStatus persondId docType fetchStatus = do
  case fetchStatus of
    DImage.APPROVED ->
      findOneWithKV
        [ Se.And
            [ Se.Is BeamI.personId $ Se.Eq $ getId persondId,
              Se.Is BeamI.imageType $ Se.Eq docType,
              Se.Is BeamI.verificationStatus $ Se.Eq (Just Documents.VALID)
            ]
        ]
    DImage.NEEDS_REVIEW ->
      findOneWithKV
        [ Se.And
            [ Se.Is BeamI.personId $ Se.Eq $ persondId.getId,
              Se.Is BeamI.imageType $ Se.Eq docType,
              Se.Is BeamI.verificationStatus $ Se.Eq (Just Documents.MANUAL_VERIFICATION_REQUIRED)
            ]
        ]

updateVerificationStatusOnlyById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Documents.VerificationStatus -> Kernel.Types.Id.Id DImage.Image -> m ())
updateVerificationStatusOnlyById verificationStatus (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set BeamI.verificationStatus (Just verificationStatus), Se.Set BeamI.updatedAt _now] [Se.Is BeamI.id $ Se.Eq id]

updateVerificationStatusByIdAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id DImage.Image -> DocumentType -> m ())
updateVerificationStatusByIdAndType verificationStatus (Kernel.Types.Id.Id id) imageType = do
  updateOneWithKV
    [Se.Set BeamI.verificationStatus (Just verificationStatus)]
    [ Se.And
        [ Se.Is BeamI.id $ Se.Eq id,
          Se.Is BeamI.imageType $ Se.Eq imageType
        ]
    ]

updateVerificationStatusAndFailureReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Documents.VerificationStatus -> DriverOnboardingError -> Kernel.Types.Id.Id DImage.Image -> m ())
updateVerificationStatusAndFailureReason verificationStatus failureReason (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamI.verificationStatus (Just verificationStatus), Se.Set BeamI.failureReason (Just failureReason), Se.Set BeamI.updatedAt _now] [Se.Is BeamI.id $ Se.Eq id]

findByPersonIdAndImageTypes ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  (Kernel.Types.Id.Id Person -> [DocumentType] -> m [DImage.Image])
findByPersonIdAndImageTypes personId imageTypes = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is BeamI.imageType $ Se.In imageTypes
        ]
    ]

findImagesByRCAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Id DM.Merchant -> Maybe Text -> Domain.Types.DocumentVerificationConfig.DocumentType -> Maybe Int -> m [DImage.Image])
findImagesByRCAndType merchantId rcId imageType mbLimit = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamI.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is BeamI.rcId $ Se.Eq rcId,
          Se.Is BeamI.imageType $ Se.Eq imageType
        ]
    ]
    (Se.Desc BeamI.createdAt)
    mbLimit
    Nothing

findRecentLatestByPersonIdAndImagesType :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> DocumentType -> m [DImage.Image]
findRecentLatestByPersonIdAndImagesType driverId imgType = do
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamI.personId $ Se.Eq driverId.getId,
          Se.Is BeamI.imageType $ Se.Eq imgType
        ]
    ]
    Nothing
    >>= \case
      [] -> return []
      images -> do
        let latestImg = DL.maximumBy (compare `on` (.createdAt)) images
        return $ filter ((== latestImg.workflowTransactionId) . (.workflowTransactionId)) images

data DriverImagesInfo = DriverImagesInfo
  { driverId :: Maybe (Id Person), -- Nothing for vehilce inspection, without driver linked to vehicle
    merchantOperatingCity :: DMOC.MerchantOperatingCity,
    driverImages :: [DImage.Image],
    transporterConfig :: DTC.TransporterConfig,
    now :: UTCTime
  }

-- filter on haskell level instead of db queries for performance reason: findRecentByPersonIdAndImageType
filterRecentByPersonIdAndImageType :: DriverImagesInfo -> DocumentType -> [DImage.Image]
filterRecentByPersonIdAndImageType DriverImagesInfo {transporterConfig, driverImages, now} imgType = do
  let onboardingRetryTimeInHours = transporterConfig.onboardingRetryTimeInHours
  filter (\img -> img.imageType == imgType && img.createdAt >= hoursAgo onboardingRetryTimeInHours now) driverImages

-- filter on haskell level instead of db queries for performance reason: findImagesByPersonAndType
filterImagesByPersonAndType ::
  DriverImagesInfo ->
  Id DM.Merchant ->
  DocumentType ->
  [DImage.Image]
filterImagesByPersonAndType DriverImagesInfo {driverImages} merchantId imageType =
  filter (\img -> img.merchantId == merchantId && img.imageType == imageType) driverImages

-- filter on haskell level instead of db queries for performance reason: findImageByPersonIdAndImageTypeAndVerificationStatus
filterImageByPersonIdAndImageTypeAndVerificationStatus :: DriverImagesInfo -> DocumentType -> [Documents.VerificationStatus] -> [DImage.Image]
filterImageByPersonIdAndImageTypeAndVerificationStatus DriverImagesInfo {driverImages} imageType verificationStatus = do
  filter (\img -> img.imageType == imageType && img.verificationStatus `elem` (Just <$> verificationStatus)) driverImages

-- filter on haskell level instead of db queries for performance reason: findRecentLatestByPersonIdAndImageType
filterRecentLatestByPersonIdAndImageType :: DriverImagesInfo -> DocumentType -> Maybe DImage.Image
filterRecentLatestByPersonIdAndImageType DriverImagesInfo {driverImages} imgType = do
  case filter (\img -> img.imageType == imgType) driverImages of
    [] -> Nothing
    images -> Just (DL.maximumBy (compare `on` (.createdAt)) images)

data RcImagesInfo = RcImagesInfo
  { rcId :: Id DReg.VehicleRegistrationCertificate,
    rcImages :: [DImage.Image],
    documentTypes :: [DocumentType]
  }

-- filter on haskell level instead of db queries for performance reason: findRecentByPersonRCAndImageType
filterRecentByPersonRCAndImageType :: RcImagesInfo -> DocumentType -> [DImage.Image]
filterRecentByPersonRCAndImageType RcImagesInfo {rcImages} imgType = do
  filter (\img -> img.imageType == imgType) rcImages
