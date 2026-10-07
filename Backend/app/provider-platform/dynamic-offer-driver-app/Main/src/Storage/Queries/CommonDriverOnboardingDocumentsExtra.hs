module Storage.Queries.CommonDriverOnboardingDocumentsExtra
  ( CommonDocumentsFilter (..),
    CommonDocumentOwner (..),
    mkCommonDocumentOwner,
    findAllForCommonDocuments,
    countForCommonDocuments,
    findCurrentVersionByOwnerAndDocumentType,
    createOrReplaceUnreviewed,
  )
where

import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Database.Beam as B
import qualified Domain.Types.CommonDriverOnboardingDocuments as DCommonDoc
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.CommonDriverOnboardingDocuments as Beam
import Storage.Queries.CommonDriverOnboardingDocuments (create, updateByPrimaryKey)

data CommonDocumentsFilter = CommonDocumentsFilter
  { merchantId :: Id.Id DM.Merchant,
    merchantOperatingCityId :: Id.Id DMOC.MerchantOperatingCity,
    driverIds :: Maybe [Id.Id DP.Person],
    documentTypes :: Maybe [DVC.DocumentType],
    verificationStatuses :: Maybe [Documents.VerificationStatus],
    from :: Maybe UTCTime,
    to :: Maybe UTCTime,
    limit :: Maybe Int,
    offset :: Maybe Int,
    sortByField :: Maybe Text,
    sortOrder :: Maybe Text
  }

buildOrderBy :: CommonDocumentsFilter -> Se.OrderBy Beam.CommonDriverOnboardingDocumentsT
buildOrderBy CommonDocumentsFilter {..} =
  case (sortByField, sortOrder) of
    (Just sb, Just so)
      | sb == "updatedAt" && so == "asc" -> Se.Asc Beam.updatedAt
      | sb == "updatedAt" -> Se.Desc Beam.updatedAt
      | sb == "createdAt" && so == "asc" -> Se.Asc Beam.createdAt
      | sb == "createdAt" -> Se.Desc Beam.createdAt
    (Just sb, _)
      | sb == "updatedAt" -> Se.Desc Beam.updatedAt
      | sb == "createdAt" -> Se.Desc Beam.createdAt
    _ -> Se.Desc Beam.createdAt

findAllForCommonDocuments ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  CommonDocumentsFilter ->
  m [DCommonDoc.CommonDriverOnboardingDocuments]
findAllForCommonDocuments flt@CommonDocumentsFilter {..} =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq (Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId merchantOperatingCityId)
        ]
          <> maybe [] (\ds -> [Se.Is Beam.driverId $ Se.In (map (Just . Id.getId) ds)]) driverIds
          <> maybe [] (\dts -> [Se.Is Beam.documentType $ Se.In dts]) documentTypes
          <> maybe [] (\vss -> [Se.Is Beam.verificationStatus $ Se.In vss]) verificationStatuses
          <> maybe [] (\f -> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq f]) from
          <> maybe [] (\t -> [Se.Is Beam.createdAt $ Se.LessThanOrEq t]) to
    ]
    (buildOrderBy flt)
    limit
    offset

countForCommonDocuments ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  CommonDocumentsFilter ->
  m Int
countForCommonDocuments CommonDocumentsFilter {..} = do
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_'
            ( \row ->
                B.sqlBool_ (Beam.merchantId row B.==. B.val_ (Id.getId merchantId))
                  B.&&?. B.sqlBool_ (Beam.merchantOperatingCityId row B.==. B.val_ (Id.getId merchantOperatingCityId))
                  B.&&?. maybe (B.sqlBool_ (B.val_ True)) (\ds -> B.sqlBool_ (Beam.driverId row `B.in_` (B.val_ . Just . Id.getId <$> ds))) driverIds
                  B.&&?. maybe (B.sqlBool_ (B.val_ True)) (\dts -> B.sqlBool_ (Beam.documentType row `B.in_` (B.val_ <$> dts))) documentTypes
                  B.&&?. maybe (B.sqlBool_ (B.val_ True)) (\vss -> B.sqlBool_ (Beam.verificationStatus row `B.in_` (B.val_ <$> vss))) verificationStatuses
                  B.&&?. maybe (B.sqlBool_ (B.val_ True)) (\f -> B.sqlBool_ (Beam.createdAt row B.>=. B.val_ f)) from
                  B.&&?. maybe (B.sqlBool_ (B.val_ True)) (\t -> B.sqlBool_ (Beam.createdAt row B.<=. B.val_ t)) to
            )
            do
              B.all_ (BeamCommon.commonDriverOnboardingDocuments BeamCommon.atlasDB)
  pure $ either (const 0) (\r -> if null r then 0 else head r) res

-- | Whom a common document belongs to. The type guarantees at least one identifier is present, so
--   the lookup can never degrade into an unscoped scan or a @driver_id IS NULL@ filter.
data CommonDocumentOwner
  = OwnedByDriver (Id.Id DP.Person)
  | OwnedByRc (Id.Id DRC.VehicleRegistrationCertificate)
  | OwnedByDriverAndRc (Id.Id DP.Person) (Id.Id DRC.VehicleRegistrationCertificate)

mkCommonDocumentOwner :: Maybe (Id.Id DP.Person) -> Maybe (Id.Id DRC.VehicleRegistrationCertificate) -> Maybe CommonDocumentOwner
mkCommonDocumentOwner mbDriverId mbRcId = case (mbDriverId, mbRcId) of
  (Just driverId, Just rcId) -> Just $ OwnedByDriverAndRc driverId rcId
  (Just driverId, Nothing) -> Just $ OwnedByDriver driverId
  (Nothing, Just rcId) -> Just $ OwnedByRc rcId
  (Nothing, Nothing) -> Nothing

-- | The version of a common document that drives its status (see 'pickCurrentVersion').
findCurrentVersionByOwnerAndDocumentType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  CommonDocumentOwner ->
  DVC.DocumentType ->
  m (Maybe DCommonDoc.CommonDriverOnboardingDocuments)
findCurrentVersionByOwnerAndDocumentType owner documentType =
  pickCurrentVersion <$> findAllVersionsByOwnerAndDocumentType owner documentType

findAllVersionsByOwnerAndDocumentType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  CommonDocumentOwner ->
  DVC.DocumentType ->
  m [DCommonDoc.CommonDriverOnboardingDocuments]
findAllVersionsByOwnerAndDocumentType owner documentType =
  findAllWithKV [Se.And $ Se.Is Beam.documentType (Se.Eq documentType) : ownerClauses]
  where
    ownerClauses = case owner of
      OwnedByDriver driverId -> [driverClause driverId]
      OwnedByRc rcId -> [rcClause rcId]
      OwnedByDriverAndRc driverId rcId -> [driverClause driverId, rcClause rcId]
    driverClause driverId = Se.Is Beam.driverId $ Se.Eq (Just $ Id.getId driverId)
    rcClause rcId = Se.Is Beam.rcId $ Se.Eq (Just $ Id.getId rcId)

-- | A text-only submission overwrites the owner's latest unreviewed row; anything else creates one.
--   TDS certificates always create: each row is a separate batch.
--   Latest = newest upload: approving or rejecting an older version rewrites its updatedAt.
createOrReplaceUnreviewed ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DCommonDoc.CommonDriverOnboardingDocuments ->
  m (Id.Id DCommonDoc.CommonDriverOnboardingDocuments)
createOrReplaceUnreviewed newDoc = do
  latest <-
    if isTextField newDoc
      then maybe (pure Nothing) (\owner -> newestUpload <$> findAllVersionsByOwnerAndDocumentType owner newDoc.documentType) (mkCommonDocumentOwner newDoc.driverId newDoc.rcId)
      else pure Nothing
  case latest of
    Just doc | isTextField doc && doc.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED && doc.rcId == newDoc.rcId -> do
      updateByPrimaryKey doc {DCommonDoc.documentData = newDoc.documentData}
      pure doc.id
    _ -> do
      create newDoc
      pure newDoc.id
  where
    isTextField :: DCommonDoc.CommonDriverOnboardingDocuments -> Bool
    isTextField doc = isNothing doc.documentImageId && doc.documentType /= DVC.TDSCertificate
    newestUpload :: [DCommonDoc.CommonDriverOnboardingDocuments] -> Maybe DCommonDoc.CommonDriverOnboardingDocuments
    newestUpload = listToMaybe . sortOn (Down . (.createdAt))

-- | The version that decides a common document's status: an approved one if any exists
--   (the most recently approved when there are several), otherwise the newest upload.
pickCurrentVersion :: [DCommonDoc.CommonDriverOnboardingDocuments] -> Maybe DCommonDoc.CommonDriverOnboardingDocuments
pickCurrentVersion = listToMaybe . sortOn (Down . rank)
  where
    rank doc =
      let approved = doc.verificationStatus == Documents.VALID
       in (approved, if approved then doc.updatedAt else doc.createdAt)
