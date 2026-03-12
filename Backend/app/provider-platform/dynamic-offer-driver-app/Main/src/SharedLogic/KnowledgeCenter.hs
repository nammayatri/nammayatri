{-
  Knowledge Center: SOP documents (images/videos) for admin dashboard.
  - Images: direct upload (base64) -> S3 put -> store s3Path in knowledge_center.
  - Videos: uploadLink (presigned URL + create KC row with s3Path) -> confirm (verify file in S3).
-}

module SharedLogic.KnowledgeCenter
  ( knowledgeCenterSopList,
    knowledgeCenterGetDocument,
    knowledgeCenterUploadImage,
    knowledgeCenterVideoUploadLink,
    knowledgeCenterVideoConfirm,
    knowledgeCenterRenameSopType,
    knowledgeCenterDeleteDocument,
    knowledgeCenterDeleteBySopType,
  )
where

import AWS.S3 as S3
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Types.KnowledgeCenter as DKC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
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

-- | List all SOP types and their document IDs for a merchant operating city.
-- Returns unique sopTypes with the list of knowledgeCenterIds for each.
knowledgeCenterSopList ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  m [(Text, [Text])]
knowledgeCenterSopList merchantShortId opCity mbMerchantOperatingCityId = do
  (_merchant, mocId) <- resolveMerchantAndCity merchantShortId opCity mbMerchantOperatingCityId
  docs <- QKC.findAllByMerchantOperatingCityId mocId
  let grouped =
        Map.toList $
          Map.fromListWith (++) $
            map (\kc -> (kc.sopType, [kc.id.getId])) docs
  pure $ sortOn fst grouped

-- | Get document for viewing: image as base64, video as presigned view link.
knowledgeCenterGetDocument ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r, HasField "s3Env" r (S3.S3Env m)) =>
  ShortId DM.Merchant ->
  Text ->
  m (S3.FileType, Maybe Text, Maybe Text)
knowledgeCenterGetDocument merchantShortId knowledgeCenterId = do
  kc <- QKC.findById (Id knowledgeCenterId) >>= fromMaybeM (InvalidRequest "KnowledgeCenter document not found")
  merchant <- findMerchantByShortId merchantShortId
  case kc.fileType of
    S3.Image -> do
      content <- S3.get (T.unpack kc.s3Path)
      pure (S3.Image, Just content, Nothing)
    S3.Video -> do
      viewLink <- S3.generateDownloadUrl (T.unpack kc.s3Path) merchant.mediaFileDocumentLinkExpires
      pure (S3.Video, Nothing, Just viewLink)
    _ ->
      throwError $ InvalidRequest "Unsupported file type for viewing (only Image and Video are supported)"

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

-- | Create S3 path for knowledge center media.
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
  pure
    ( pathPrefix <> "/knowledge-center/mocId-"
        <> merchantOpCityId.getId
        <> "/"
        <> T.take 200 sopType
        <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> "."
        <> ext
    )

-- | Allowed image extensions for Knowledge Center (whitelist).
allowedImageExtensions :: [Text]
allowedImageExtensions = ["jpg", "jpeg", "png", "pdf"]

-- | Upload image (base64) and create KnowledgeCenter row.
knowledgeCenterUploadImage ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r, HasField "s3Env" r (S3.S3Env m)) =>
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  m (Id DKC.KnowledgeCenter)
knowledgeCenterUploadImage merchantShortId opCity sopType imageBase64 fileExtension mbMocId = do
  (merchant, merchantOpCityId) <- resolveMerchantAndCity merchantShortId opCity mbMocId
  imageBytes <- fromMaybeM (InvalidRequest "Failed to decode base64 image") $ base64Decode imageBase64

  -- Size limit (same as driver document upload: maxAllowedDocSizeInMB, default 100 MB)
  transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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

  -- Extension sanitization (same as driver document: strip, drop leading '.', toLower, default "png")
  let sanitizedExt =
        case fileExtension of
          Nothing -> "png"
          Just ext ->
            let t = T.strip $ T.dropWhile (== '.') ext
             in if T.null t then "png" else T.toLower t

  -- Only .jpg, .jpeg, .png and .pdf allowed for image
  unless (sanitizedExt `elem` allowedImageExtensions) $
    throwError $
      InvalidRequest "Only .jpg, .jpeg, .png and .pdf files are allowed for image upload"

  s3Path <- createKnowledgeCenterPath merchantOpCityId sopType S3.Image sanitizedExt
  _ <- fork "S3 Put KnowledgeCenter Image" $ S3.put (T.unpack s3Path) imageBase64
  kcId <- generateGUID
  now <- getCurrentTime
  let kc =
        DKC.KnowledgeCenter
          { id = Id kcId,
            fileType = S3.Image,
            sopType,
            merchantOperatingCityId = Just merchantOpCityId,
            s3Path,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchant.id
          }
  QKC.create kc
  pure (Id kcId)

-- | Get presigned upload URL for video and create KnowledgeCenter row (s3Path set; file not yet uploaded).
knowledgeCenterVideoUploadLink ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r, HasField "s3Env" r (S3.S3Env m)) =>
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Maybe Text ->
  m (Maybe Text, Id DKC.KnowledgeCenter)
knowledgeCenterVideoUploadLink merchantShortId opCity sopType reqContentType mbMocId = do
  (merchant, merchantOpCityId) <- resolveMerchantAndCity merchantShortId opCity mbMocId
  ext <-
    case reqContentType of
      "video/mp4" -> pure "mp4"
      "video/x-msvideo" -> pure "avi"
      _ -> throwError $ InvalidRequest $ "Unsupported video content type: " <> reqContentType
  s3Path <- createKnowledgeCenterPath merchantOpCityId sopType S3.Video ext
  uploadLink <- S3.generateUploadUrl (T.unpack s3Path) merchant.mediaFileDocumentLinkExpires
  kcId <- generateGUID
  now <- getCurrentTime
  let kc =
        DKC.KnowledgeCenter
          { id = Id kcId,
            fileType = S3.Video,
            sopType,
            merchantOperatingCityId = Just merchantOpCityId,
            s3Path,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchant.id
          }
  QKC.create kc
  pure (Just uploadLink, Id kcId)

-- | Confirm video upload: verify file exists in S3 and size within limit (row already created in uploadLink).
knowledgeCenterVideoConfirm ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r, HasField "s3Env" r (S3.S3Env m)) =>
  Text ->
  m APISuccess
knowledgeCenterVideoConfirm knowledgeCenterId = do
  kc <- QKC.findById (Id knowledgeCenterId) >>= fromMaybeM (InvalidRequest "KnowledgeCenter document not found")
  unless (kc.fileType == S3.Video) $ throwError (InvalidRequest "Document is not a video")

  s3ObjectStatus <-
    catch (S3.headRequest (T.unpack kc.s3Path)) $ \(_err :: SomeException) ->
      throwError (InvalidRequest "Video file was not found in storage. Please upload and try again.")

  -- Video size limit (same as MediaFileDocument: maxAllowedVideoDocSizeInMB, default 500 MB)
  whenJust kc.merchantOperatingCityId $ \mocId -> do
    transporterConfig <- CCT.findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
    let maxSizeInMB = fromMaybe 500 transporterConfig.maxAllowedVideoDocSizeInMB
        maxSizeInBytes = toInteger maxSizeInMB * 1024 * 1024
    when (s3ObjectStatus.fileSizeInBytes > maxSizeInBytes) $
      throwError $
        InvalidRequest $
          "Video size " <> show s3ObjectStatus.fileSizeInBytes <> " bytes exceeds maximum limit of "
            <> show maxSizeInBytes
            <> " bytes ("
            <> show maxSizeInMB
            <> "MB)"

  pure Success

-- | Rename sopType: update all rows with oldSopType to newSopType.
knowledgeCenterRenameSopType ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Text ->
  Text ->
  m APISuccess
knowledgeCenterRenameSopType newSopType oldSopType = do
  rows <- QKC.findAllBySopType (Just 10000) (Just 0) oldSopType
  now <- getCurrentTime
  forM_ rows $ \r ->
    QKC.updateByPrimaryKey r {DKC.sopType = newSopType, DKC.updatedAt = now}
  pure Success

-- | Delete one document by id.
knowledgeCenterDeleteDocument ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Text ->
  m APISuccess
knowledgeCenterDeleteDocument knowledgeCenterId = do
  QKC.deleteById (Id knowledgeCenterId)
  pure Success

-- | Delete all documents for a sopType.
knowledgeCenterDeleteBySopType ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Text ->
  m APISuccess
knowledgeCenterDeleteBySopType sopType = do
  QKC.deleteBySopType sopType
  pure Success
