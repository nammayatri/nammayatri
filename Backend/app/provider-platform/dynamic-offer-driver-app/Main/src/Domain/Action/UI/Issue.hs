{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Issue where

import qualified AWS.S3 as S3
import qualified Data.ByteString as BS
import qualified Domain.Types.Issue.IssueReport as D
import qualified Domain.Types.Issue.IssueCategory as D
import qualified Domain.Types.Issue.IssueOption as D
import qualified Domain.Types.Issue.IssueTranslation as D
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Message.MediaFile as D
import qualified Domain.Types.Person as SP
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Text as T hiding (map)
import qualified EulerHS.Language as L
import EulerHS.Types (base64Encode)
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess(Success))
import Kernel.Types.Id
import Kernel.External.Types (Language(ENGLISH))
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Utils.Common
import qualified "shared-services" SharedService.ProviderPlatform.Issue as Common
import qualified Storage.Queries.Issue.IssueReport as QIR
import qualified Storage.Queries.Message.MediaFile as QMF
import qualified Storage.CachedQueries.Issue.IssueCategory as CQIC
import qualified Storage.CachedQueries.Issue.IssueOption as CQIO
import Tools.Error

getIssueCategory :: Id SP.Person -> Maybe Language -> Flow Common.IssueCategoryListRes
getIssueCategory _driverId language = do
  issueCategoryTranslationList <- CQIC.findByLanguage (fromMaybe ENGLISH language)
  pure $ Common.IssueCategoryListRes { categories = mkIssueCategory <$> issueCategoryTranslationList }
  where
    mkIssueCategory :: (D.IssueCategory, Maybe D.IssueTranslation) -> Common.IssueCategoryRes
    mkIssueCategory (issueCategory, issueTranslation) =
      Common.IssueCategoryRes
        { issueCategoryId = cast issueCategory.id,
          category = fromMaybe issueCategory.category $ (.translation) <$> issueTranslation,
          logoUrl = issueCategory.logoUrl
        }

getIssueOption :: Id SP.Person -> Id D.IssueCategory -> Maybe Language -> Flow Common.IssueOptionListRes
getIssueOption _driverId issueCategoryId language = do
  issueOptionTranslationList <- CQIO.findAllByCategoryAndLanguage issueCategoryId (fromMaybe ENGLISH language)
  pure $ Common.IssueOptionListRes { options = mkIssueOptionList <$> issueOptionTranslationList }
  where
    mkIssueOptionList :: (D.IssueOption, Maybe D.IssueTranslation) -> Common.IssueOptionRes
    mkIssueOptionList (issueOption, issueTranslation) =
      Common.IssueOptionRes
        { issueOptionId = cast issueOption.id
        , option = fromMaybe issueOption.option $ (.translation) <$> issueTranslation
        }

issueReportDriverList :: Id SP.Person -> Flow Common.IssueReportDriverListRes
issueReportDriverList driverId = do
  issueReports <- Esq.runInReplica $ QIR.findAllByDriver driverId
  issues <- mapM mkIssueReport issueReports
  return $ Common.IssueReportDriverListRes { issues }
  where
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
    toCommonIssueStatus :: D.IssueStatus -> Common.IssueStatus
    toCommonIssueStatus = \case
      D.NEW -> Common.NEW
      D.INPROGRESS -> Common.INPROGRESS
      D.RESOLVED -> Common.RESOLVED
    mkIssueReport :: D.IssueReport -> Flow Common.IssueReportDriverListItem
    mkIssueReport issueReport = do
      mediaFiles <- Esq.runInReplica $ QMF.findAllIn issueReport.mediaFiles
      pure $
        Common.IssueReportDriverListItem
          { issueReportId = cast issueReport.id,
            category = issueReport.category,
            option = issueReport.option,
            assignee = issueReport.assignee,
            description = issueReport.description,
            status = toCommonIssueStatus issueReport.status,
            mediaFiles = mkMediaFiles mediaFiles
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
  return $ Common.IssueMediaUploadRes { fileId = cast $ fileEntity.id }
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
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- createFilePath driverId.getId fileType ""
  mediaFileUrlPattern <- asks (.mediaFileUrlPattern)
  let fileUrl = 
        mediaFileUrlPattern
          & T.replace "<DOMAIN>" "issue"
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "S3 Put Issue Media File" $ S3.put (T.unpack filePath) mediaFile
  createMediaEntry fileUrl fileType

fetchMedia :: Id SP.Person -> Text -> Flow Text
fetchMedia _driverId filePath =
  S3.get $ T.unpack filePath

createIssueReport :: Id SP.Person -> Common.IssueReportReq -> Flow Common.IssueReportRes
createIssueReport driverId Common.IssueReportReq {..} = do
  issueReport <- mkIssueReport
  Esq.runTransaction $ QIR.create issueReport
  pure $ Common.IssueReportRes { issueReportId = cast issueReport.id }
  where
    mkIssueReport = do
      id <- generateGUID
      now <- getCurrentTime
      pure $
        D.IssueReport
          { rideId = cast @Common.Ride @DRide.Ride <$> rideId,
            mediaFiles = cast @Common.MediaFile @D.MediaFile <$> mediaFiles,
            assignee = Nothing,
            status = D.NEW,
            deleted = False,
            createdAt = now,
            updatedAt = now,
            ..
          }
  
updateIssue :: Id D.IssueReport -> Id SP.Person -> Common.IssueUpdateReq -> Flow APISuccess
updateIssue issueReportId _driverId Common.IssueUpdateReq {..} = do
  when (isNothing option) $
    throwError $ InvalidRequest "Option Not Found in request body."
  whenJust option $ \justOption -> 
    Esq.runTransaction $ QIR.updateOption issueReportId justOption
  pure Success

deleteIssue :: Id D.IssueReport -> Id SP.Person -> Flow APISuccess
deleteIssue issueReportId driverId = do
  Esq.runTransaction $ QIR.updateAsDeleted issueReportId driverId
  pure Success
  