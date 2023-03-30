{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Issue where

import qualified AWS.S3 as S3
import qualified Data.ByteString as BS
import Data.Text as T hiding (map)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Types.Issue.IssueCategory as D
import qualified Domain.Types.Issue.IssueOption as D
import qualified Domain.Types.Issue.IssueReport as D
import qualified Domain.Types.Issue.IssueTranslation as D
import qualified Domain.Types.MediaFile as D
import qualified Domain.Types.Person as SP
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "shared-services" SharedService.ProviderPlatform.Issue as Common
import qualified Storage.CachedQueries.Issue.IssueCategory as CQIC
import qualified Storage.CachedQueries.Issue.IssueOption as CQIO
import qualified Storage.CachedQueries.Issue.IssueReport as CQIR
import qualified Storage.CachedQueries.MediaFile as CQMF
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.Queries.Issue.IssueReport as QIR
import qualified Storage.Queries.MediaFile as QMF
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error

toCommonIssueStatus :: D.IssueStatus -> Common.IssueStatus
toCommonIssueStatus = \case
  D.NEW -> Common.NEW
  D.INPROGRESS -> Common.INPROGRESS
  D.RESOLVED -> Common.RESOLVED

getLanguage :: Id SP.Person -> Maybe Language -> Flow Language
getLanguage driverId mbLanguage = do
  extractLanguage <-
    if isJust mbLanguage
      then return mbLanguage
      else runMaybeT $ do
        driverDetail <- MaybeT . Esq.runInReplica $ QP.findById driverId
        MaybeT $ pure driverDetail.language
  return $ fromMaybe ENGLISH extractLanguage

getIssueCategory :: Id SP.Person -> Maybe Language -> Flow Common.IssueCategoryListRes
getIssueCategory driverId mbLanguage = do
  language <- getLanguage driverId mbLanguage
  issueCategoryTranslationList <- CQIC.findAllByLanguage language
  pure $ Common.IssueCategoryListRes {categories = mkIssueCategory <$> issueCategoryTranslationList}
  where
    mkIssueCategory :: (D.IssueCategory, Maybe D.IssueTranslation) -> Common.IssueCategoryRes
    mkIssueCategory (issueCategory, issueTranslation) =
      Common.IssueCategoryRes
        { issueCategoryId = cast issueCategory.id,
          label = issueCategory.category & T.toUpper & T.replace " " "_",
          category = fromMaybe issueCategory.category $ issueTranslation <&> (.translation),
          logoUrl = issueCategory.logoUrl
        }

getIssueOption :: Id SP.Person -> Id D.IssueCategory -> Maybe Language -> Flow Common.IssueOptionListRes
getIssueOption driverId issueCategoryId mbLanguage = do
  language <- getLanguage driverId mbLanguage
  issueOptionTranslationList <- CQIO.findAllByCategoryAndLanguage issueCategoryId language
  pure $ Common.IssueOptionListRes {options = mkIssueOptionList <$> issueOptionTranslationList}
  where
    mkIssueOptionList :: (D.IssueOption, Maybe D.IssueTranslation) -> Common.IssueOptionRes
    mkIssueOptionList (issueOption, issueTranslation) =
      Common.IssueOptionRes
        { issueOptionId = cast issueOption.id,
          label = issueOption.option & T.toUpper & T.replace " " "_",
          option = fromMaybe issueOption.option $ issueTranslation <&> (.translation)
        }

issueReportDriverList :: Id SP.Person -> Maybe Language -> Flow Common.IssueReportDriverListRes
issueReportDriverList driverId language = do
  issueReports <- CQIR.findAllByDriver driverId
  issues <- mapM mkIssueReport issueReports
  return $ Common.IssueReportDriverListRes {issues}
  where
    mkIssueReport :: D.IssueReport -> Flow Common.IssueReportDriverListItem
    mkIssueReport issueReport = do
      (issueCategory, issueCategoryTranslation) <- CQIC.findByIdAndLanguage issueReport.categoryId (fromMaybe ENGLISH language) >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
      return $
        Common.IssueReportDriverListItem
          { issueReportId = cast issueReport.id,
            category = fromMaybe issueCategory.category $ issueCategoryTranslation <&> (.translation),
            status = toCommonIssueStatus issueReport.status,
            createdAt = issueReport.createdAt
          }

createFilePath :: Text -> Common.FileType -> Text -> Flow Text
createFilePath driverId fileType validatedFileExtention = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "issue-media/" <> "driver-" <> driverId <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> validatedFileExtention
    )

createMediaEntry :: Text -> Common.FileType -> Flow Common.IssueMediaUploadRes
createMediaEntry url fileType = do
  fileEntity <- mkFile url
  Esq.runTransaction $ QMF.create fileEntity
  return $ Common.IssueMediaUploadRes {fileId = cast $ fileEntity.id}
  where
    mapToMediaFileType = \case
      Common.Audio -> D.Audio
      Common.Image -> D.Image
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        D.MediaFile
          { id,
            _type = mapToMediaFileType fileType,
            url = fileUrl,
            createdAt = now
          }

issueMediaUpload :: Id SP.Person -> Common.IssueMediaUploadReq -> Flow Common.IssueMediaUploadRes
issueMediaUpload driverId Common.IssueMediaUploadReq {..} = do
  contentType <- validateContentType
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  person <- Esq.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  transporterConfig <- CQTC.findByMerchantId person.merchantId >>= fromMaybeM (TransporterConfigNotFound person.merchantId.getId)
  when (fileSize > fromIntegral transporterConfig.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- createFilePath driverId.getId fileType contentType
  let fileUrl =
        transporterConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "issue"
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "S3 Put Issue Media File" $ S3.put (T.unpack filePath) mediaFile
  createMediaEntry fileUrl fileType
  where
    validateContentType = do
      case fileType of
        Common.Audio | reqContentType == "audio/mpeg" -> pure "mp3"
        Common.Audio | reqContentType == "audio/mp4" -> pure "mp4"
        Common.Image | reqContentType == "image/png" -> pure "png"
        Common.Image | reqContentType == "image/jpeg" -> pure "jpg"
        _ -> throwError $ FileFormatNotSupported reqContentType

fetchMedia :: Id SP.Person -> Text -> Flow Text
fetchMedia _driverId filePath =
  S3.get $ T.unpack filePath

createIssueReport :: Id SP.Person -> Common.IssueReportReq -> Flow Common.IssueReportRes
createIssueReport driverId Common.IssueReportReq {..} = do
  void $ CQIC.findById (cast categoryId) >>= fromMaybeM (IssueCategoryDoNotExist categoryId.getId)
  whenJust optionId $ \justOptionId ->
    void $ CQIO.findByIdAndCategoryId (cast justOptionId) (cast categoryId) >>= fromMaybeM (IssueOptionInvalid justOptionId.getId categoryId.getId)
  whenJust rideId $ \justRideId ->
    void $ Esq.runInReplica (QRide.findById $ cast justRideId) >>= fromMaybeM (RideNotFound justRideId.getId)
  forM_ mediaFiles $ \mediaFile ->
    void $ CQMF.findById (cast mediaFile) >>= fromMaybeM (FileDoNotExist mediaFile.getId)
  issueReport <- mkIssueReport
  Esq.runTransaction $ QIR.create issueReport
  CQIR.invalidateIssueReportCache Nothing (Just driverId)
  pure $ Common.IssueReportRes {issueReportId = cast issueReport.id}
  where
    mkIssueReport = do
      id <- generateGUID
      now <- getCurrentTime
      pure $
        D.IssueReport
          { id,
            driverId,
            rideId = cast <$> rideId,
            optionId = cast <$> optionId,
            categoryId = cast categoryId,
            mediaFiles = cast <$> mediaFiles,
            assignee = Nothing,
            status = D.NEW,
            deleted = False,
            createdAt = now,
            updatedAt = now,
            description
          }

issueInfo :: Id D.IssueReport -> Id SP.Person -> Maybe Language -> Flow Common.IssueInfoRes
issueInfo issueReportId driverId mbLanguage = do
  language <- getLanguage driverId mbLanguage
  issueReport <- CQIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  mediaFiles <- CQMF.findAllInForIssueReportId issueReport.mediaFiles issueReportId
  (issueCategory, issueCategoryTranslation) <- CQIC.findByIdAndLanguage issueReport.categoryId language >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
  mbIssueOption <- (join <$>) $
    forM issueReport.optionId $ \justIssueOption -> do
      CQIO.findByIdAndLanguage justIssueOption language
  pure $
    Common.IssueInfoRes
      { issueReportId = cast issueReport.id,
        category = fromMaybe issueCategory.category $ (.translation) <$> issueCategoryTranslation,
        option = mkIssueOption <$> mbIssueOption,
        assignee = issueReport.assignee,
        description = issueReport.description,
        status = toCommonIssueStatus issueReport.status,
        mediaFiles = mkMediaFiles mediaFiles,
        createdAt = issueReport.createdAt
      }
  where
    mkIssueOption :: (D.IssueOption, Maybe D.IssueTranslation) -> Text
    mkIssueOption (issueOption, issueOptionTranslation) =
      fromMaybe issueOption.option $ (.translation) <$> issueOptionTranslation
    mkMediaFiles :: [D.MediaFile] -> [Common.MediaFile_]
    mkMediaFiles =
      foldr'
        ( \mediaFile mediaFileList -> do
            case mediaFile._type of
              D.Audio -> Common.MediaFile_ Common.Audio mediaFile.url : mediaFileList
              D.Image -> Common.MediaFile_ Common.Image mediaFile.url : mediaFileList
              _ -> mediaFileList
        )
        []

updateIssueOption :: Id D.IssueReport -> Id SP.Person -> Common.IssueUpdateReq -> Flow APISuccess
updateIssueOption issueReportId driverId Common.IssueUpdateReq {..} = do
  void $ CQIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  void $ CQIO.findByIdAndCategoryId (cast optionId) (cast categoryId) >>= fromMaybeM (IssueOptionInvalid optionId.getId categoryId.getId)
  Esq.runTransaction $ QIR.updateOption issueReportId (cast optionId)
  CQIR.invalidateIssueReportCache (Just issueReportId) (Just driverId)
  pure Success

deleteIssue :: Id D.IssueReport -> Id SP.Person -> Flow APISuccess
deleteIssue issueReportId driverId = do
  unlessM (Esq.runInReplica (QIR.isSafeToDelete issueReportId driverId)) $
    throwError (InvalidRequest "This issue is either already deleted, or is not associated to this driver.")
  issueReport <- CQIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  Esq.runTransaction $ QIR.updateAsDeleted issueReportId
  CQIR.invalidateIssueReportCache (Just issueReportId) (Just driverId)
  CQMF.invalidateMediaFileCache issueReport.mediaFiles (Just issueReportId)
  pure Success
