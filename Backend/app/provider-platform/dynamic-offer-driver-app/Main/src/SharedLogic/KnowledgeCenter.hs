{-
  Knowledge Center: SOP documents (images and PDFs).
  - Images/PDF: direct upload (base64) -> S3 put -> store s3Path and fileType in knowledge_center.
  - SOP type names are stored as an array in transporter config (per merchantOpCity); documents are queried from knowledge_center table by sopType + merchantOpCityId.
-}

module SharedLogic.KnowledgeCenter
  ( knowledgeCenterSopList,
    knowledgeCenterGetDocument,
    knowledgeCenterUploadImage,
    knowledgeCenterRenameSopType,
    knowledgeCenterDeleteDocument,
    knowledgeCenterDeleteBySopType,
  )
where

import AWS.S3 as S3
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Types.Extra.TransporterConfig as Extra
import qualified Domain.Types.KnowledgeCenter as DKC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.TransporterConfig (TransporterConfig, TransporterConfigD (..))
import EulerHS.Types (base64Decode)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.KnowledgeCenter as QKC
import Tools.Error

maxKnowledgeCenterDocsPerSopType :: Int
maxKnowledgeCenterDocsPerSopType = 10000

knowledgeCenterSopList ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  m [(Text, [(Text, Text)])]
knowledgeCenterSopList merchantShortId opCity mbMerchantOperatingCityId mbSopType = do
  (_merchant, mocId) <- resolveMerchantAndCity merchantShortId opCity mbMerchantOperatingCityId
  transporterConfig <- CCT.findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  let sopTypes = Extra.unKnowledgeCenterSopTypesConfig transporterConfig.knowledgeCenterSopTypes
  let filteredTypes = case mbSopType of
        Nothing -> sopTypes
        Just st -> if st `elem` sopTypes then [st] else []
  -- P2-8 FIX: When sopType is omitted, fetch documents for ALL SOP types
  -- (previously returned empty document lists when sopType was omitted).
  pairs <- forM filteredTypes $ \sopType -> do
    rows <- QKC.findAllBySopTypeAndMerchantOperatingCityId (Just maxKnowledgeCenterDocsPerSopType) (Just 0) sopType mocId
    let docs = map (\r -> (r.id.getId, fromMaybe "" r.documentName)) rows
    pure (sopType, docs)
  pure $ sortOn fst pairs

knowledgeCenterGetDocument ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r, HasField "s3Env" r (S3.S3Env m)) =>
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  m (S3.FileType, Maybe Text, Maybe Text, Text)
knowledgeCenterGetDocument merchantShortId opCity knowledgeCenterId = do
  (_merchant, mocId) <- resolveMerchantAndCity merchantShortId opCity Nothing
  kc <- QKC.findById (Id knowledgeCenterId) >>= fromMaybeM (InvalidRequest "KnowledgeCenter document not found")
  unless (kc.merchantOperatingCityId == mocId) $
    throwError $ InvalidRequest "KnowledgeCenter document not found"
  case kc.fileType of
    S3.Image -> do
      content <- S3.get (T.unpack kc.s3Path)
      pure (S3.Image, Just content, Nothing, fromMaybe "" kc.documentName)
    S3.PDF -> do
      content <- S3.get (T.unpack kc.s3Path)
      pure (S3.PDF, Just content, Nothing, fromMaybe "" kc.documentName)
    S3.Video ->
      throwError $ InvalidRequest "Video viewing is not supported in this flow"
    _ ->
      throwError $ InvalidRequest "Unsupported file type for viewing (only Image and PDF are supported)"

-- | Resolve merchant and optional operating city (from path city or request).
resolveMerchantAndCity ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  m (DM.Merchant, Id DMOC.MerchantOperatingCity)
resolveMerchantAndCity merchantShortId opCity mbMocIdText = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    case mbMocIdText of
      Just mocIdT
        | not (T.null mocIdT) ->
          CQMOC.findById (Id mocIdT) >>= fromMaybeM (InvalidRequest "Invalid merchantOperatingCityId")
      _ -> CQMOC.getMerchantOpCity merchant (Just opCity)
  pure (merchant, merchantOpCity.id)

sanitizeSopTypeForS3Path :: Text -> Text
sanitizeSopTypeForS3Path = T.take 200 . T.intercalate "-" . T.words

createKnowledgeCenterPath ::
  (MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
  Id DMOC.MerchantOperatingCity ->
  Text ->
  S3.FileType ->
  Text ->
  m Text
createKnowledgeCenterPath merchantOpCityId sopType fileType extension = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
      ext = if T.null extension then ("png" :: Text) else T.toLower (T.strip extension)
      sopTypeSegment = sanitizeSopTypeForS3Path sopType
  pure
    ( pathPrefix <> "/knowledge-center/mocId-"
        <> merchantOpCityId.getId
        <> "/"
        <> sopTypeSegment
        <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> "."
        <> ext
    )

updateKnowledgeCenterSopConfigAndClearCache ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r, HasField "requestId" r (Maybe Text)) =>
  Id DMOC.MerchantOperatingCity ->
  TransporterConfig ->
  [Text] ->
  m ()
updateKnowledgeCenterSopConfigAndClearCache mocId transporterConfig newSopTypes = do
  CCT.update transporterConfig {knowledgeCenterSopTypes = Extra.KnowledgeCenterSopTypesConfig newSopTypes}
  CCT.clearCache mocId

allowedDocumentExtensions :: [Text]
allowedDocumentExtensions = ["jpg", "jpeg", "png", "pdf"]

extensionToFileType :: Text -> S3.FileType
extensionToFileType ext
  | ext == "pdf" = S3.PDF
  | otherwise = S3.Image

knowledgeCenterUploadImage ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r, HasField "s3Env" r (S3.S3Env m)) =>
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m (Maybe (Id DKC.KnowledgeCenter))
knowledgeCenterUploadImage merchantShortId opCity sopType mbImageBase64 mbDocumentName mbFileExtension mbMocId = do
  (merchant, merchantOpCityId) <- resolveMerchantAndCity merchantShortId opCity mbMocId
  transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let sopTypes = Extra.unKnowledgeCenterSopTypesConfig transporterConfig.knowledgeCenterSopTypes

  case mbImageBase64 of
    Nothing -> do
      when (sopType `elem` sopTypes) $
        throwError $ InvalidRequest "SOP type already exists"
      updateKnowledgeCenterSopConfigAndClearCache merchantOpCityId transporterConfig (sopType : sopTypes)
      pure Nothing
    Just imageBase64
      | T.null (T.strip imageBase64) -> do
        when (sopType `elem` sopTypes) $
          throwError $ InvalidRequest "SOP type already exists"
        updateKnowledgeCenterSopConfigAndClearCache merchantOpCityId transporterConfig (sopType : sopTypes)
        pure Nothing
    Just imageBase64 -> do
      let documentName = fromMaybe "" mbDocumentName
      imageBytes <- fromMaybeM (InvalidRequest "Failed to decode base64 image") $ base64Decode imageBase64
      let maxSizeInMB = fromMaybe 100 transporterConfig.maxAllowedDocSizeInMB
          maxSizeInBytes = toInteger maxSizeInMB * 1024 * 1024
      when (toInteger (BS.length imageBytes) > maxSizeInBytes) $
        throwError $
          InvalidRequest $
            "Image size " <> show (BS.length imageBytes) <> " bytes exceeds maximum limit of "
              <> show maxSizeInBytes
              <> " bytes ("
              <> show maxSizeInMB
              <> "MB)"

      let sanitizedExt =
            case mbFileExtension of
              Nothing -> "png"
              Just ext ->
                let t = T.strip $ T.dropWhile (== '.') ext
                 in if T.null t then "png" else T.toLower t
      unless (sanitizedExt `elem` allowedDocumentExtensions) $
        throwError $
          InvalidRequest "Only .jpg, .jpeg, .png and .pdf files are allowed for document upload"

      let fileType = extensionToFileType sanitizedExt
      s3Path <- createKnowledgeCenterPath merchantOpCityId sopType fileType sanitizedExt
      S3.put (T.unpack s3Path) imageBase64
      kcId <- generateGUID
      now <- getCurrentTime
      let kc =
            DKC.KnowledgeCenter
              { id = Id kcId,
                fileType,
                sopType,
                documentName = Just documentName,
                merchantOperatingCityId = merchantOpCityId,
                s3Path,
                createdAt = now,
                updatedAt = now,
                merchantId = Just merchant.id
              }
      QKC.create kc
      when (not (sopType `elem` sopTypes)) $
        updateKnowledgeCenterSopConfigAndClearCache merchantOpCityId transporterConfig (sopType : sopTypes)
      pure (Just (Id kcId))

knowledgeCenterRenameSopType ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  m APISuccess
knowledgeCenterRenameSopType merchantShortId opCity newSopType oldSopType =
  if oldSopType == newSopType
    then pure Success
    else do
      (_merchant, mocId) <- resolveMerchantAndCity merchantShortId opCity Nothing
      transporterConfig <- CCT.findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
      let sopTypes = Extra.unKnowledgeCenterSopTypesConfig transporterConfig.knowledgeCenterSopTypes
      when (newSopType `elem` sopTypes) $
        throwError $ InvalidRequest "SOP type already exists; choose a different name or merge documents first"
      rows <- QKC.findAllBySopTypeAndMerchantOperatingCityId (Just maxKnowledgeCenterDocsPerSopType) (Just 0) oldSopType mocId
      now <- getCurrentTime
      forM_ rows $ \r ->
        QKC.updateByPrimaryKey r {DKC.sopType = newSopType, DKC.updatedAt = now}
      let updatedSopTypes = map (\t -> if t == oldSopType then newSopType else t) sopTypes
      when (oldSopType `elem` sopTypes) $
        updateKnowledgeCenterSopConfigAndClearCache mocId transporterConfig updatedSopTypes
      pure Success

knowledgeCenterDeleteDocument ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r, HasField "s3Env" r (S3.S3Env m)) =>
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  m APISuccess
knowledgeCenterDeleteDocument merchantShortId opCity knowledgeCenterId = do
  (_merchant, mocId) <- resolveMerchantAndCity merchantShortId opCity Nothing
  kc <- QKC.findById (Id knowledgeCenterId) >>= fromMaybeM (InvalidRequest "KnowledgeCenter document not found")
  unless (kc.merchantOperatingCityId == mocId) $
    throwError $ InvalidRequest "KnowledgeCenter document not found"
  catch (S3.delete (T.unpack kc.s3Path)) $ \(err :: SomeException) ->
    logWarning $ "KnowledgeCenter: S3 delete failed for " <> kc.s3Path <> ": " <> show err
  QKC.deleteById (Id knowledgeCenterId)
  pure Success

knowledgeCenterDeleteBySopType ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r, HasField "s3Env" r (S3.S3Env m)) =>
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  m APISuccess
knowledgeCenterDeleteBySopType merchantShortId opCity sopType = do
  (_merchant, mocId) <- resolveMerchantAndCity merchantShortId opCity Nothing
  rows <- QKC.findAllBySopTypeAndMerchantOperatingCityId (Just maxKnowledgeCenterDocsPerSopType) (Just 0) sopType mocId
  forM_ rows $ \r ->
    catch (S3.delete (T.unpack r.s3Path)) $ \(err :: SomeException) ->
      logWarning $ "KnowledgeCenter: S3 delete failed for " <> r.s3Path <> ": " <> show err
  QKC.deleteBySopTypeAndMerchantOperatingCityId sopType mocId
  transporterConfig <- CCT.findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  let sopTypes = Extra.unKnowledgeCenterSopTypesConfig transporterConfig.knowledgeCenterSopTypes
  when (sopType `elem` sopTypes) $ do
    let updatedSopTypes = filter (/= sopType) sopTypes
    updateKnowledgeCenterSopConfigAndClearCache mocId transporterConfig updatedSopTypes
  pure Success
