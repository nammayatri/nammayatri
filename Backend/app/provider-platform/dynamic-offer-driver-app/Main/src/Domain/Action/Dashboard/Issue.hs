module Domain.Action.Dashboard.Issue where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Issue as Common
import qualified Data.Text as T hiding (map)
import qualified Domain.Types.Issue.Comment as DC
import qualified Domain.Types.Issue.IssueCategory as DIC
import qualified Domain.Types.Issue.IssueReport as DIR
import qualified Domain.Types.Issue.IssueTranslation as DIT
import qualified Domain.Types.MediaFile as DMF
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Issue.IssueCategory as CQIC
import qualified Storage.CachedQueries.Issue.IssueOption as CQIO
import qualified Storage.CachedQueries.Issue.IssueReport as CQIR
import qualified Storage.CachedQueries.MediaFile as CQMF
import qualified Storage.Queries.Issue.Comment as QC
import qualified Storage.Queries.Issue.IssueReport as QIR
import qualified Storage.Queries.Person as QP
import Tools.Error

issueCategoryList :: ShortId DM.Merchant -> Flow Common.IssueCategoryListRes
issueCategoryList _merchantShortId = do
  issueCategoryTranslationList <- CQIC.findAllByLanguage ENGLISH
  pure $ Common.IssueCategoryListRes {categories = mkIssueCategory <$> issueCategoryTranslationList}
  where
    mkIssueCategory :: (DIC.IssueCategory, Maybe DIT.IssueTranslation) -> Common.IssueCategoryRes
    mkIssueCategory (issueCategory, issueTranslation) =
      Common.IssueCategoryRes
        { issueCategoryId = cast issueCategory.id,
          label = issueCategory.category & T.toUpper & T.replace " " "_",
          category = fromMaybe issueCategory.category $ issueTranslation <&> (.translation)
        }

toDomainIssueStatus :: Common.IssueStatus -> DIR.IssueStatus
toDomainIssueStatus = \case
  Common.NEW -> DIR.NEW
  Common.INPROGRESS -> DIR.INPROGRESS
  Common.RESOLVED -> DIR.RESOLVED

toCommonIssueStatus :: DIR.IssueStatus -> Common.IssueStatus
toCommonIssueStatus = \case
  DIR.NEW -> Common.NEW
  DIR.INPROGRESS -> Common.INPROGRESS
  DIR.RESOLVED -> Common.RESOLVED

issueList :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Common.IssueStatus -> Maybe (Id DIC.IssueCategory) -> Maybe Text -> Flow Common.IssueReportListResponse
issueList _merchantShortId mbLimit mbOffset mbStatus mbCategoryId mbAssignee = do
  issueReports <- Esq.runInReplica $ QIR.findAllWithLimitOffsetStatus mbLimit mbOffset (toDomainIssueStatus <$> mbStatus) mbCategoryId mbAssignee
  let count = length issueReports
  let summary = Common.Summary {totalCount = count, count}
  issues <- mapM mkIssueReport issueReports
  return $ Common.IssueReportListResponse {issues, summary}
  where
    mkIssueReport :: DIR.IssueReport -> Flow Common.IssueReportListItem
    mkIssueReport issueReport = do
      category <- CQIC.findById issueReport.categoryId >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
      pure $
        Common.IssueReportListItem
          { issueReportId = cast issueReport.id,
            driverId = cast issueReport.driverId,
            rideId = cast <$> issueReport.rideId,
            category = category.category,
            assignee = issueReport.assignee,
            status = toCommonIssueStatus issueReport.status,
            createdAt = issueReport.createdAt
          }

issueInfo :: ShortId DM.Merchant -> Id DIR.IssueReport -> Flow Common.IssueInfoRes
issueInfo _merchantShortId issueReportId = do
  issueReport <- Esq.runInReplica $ QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  mediaFiles <- CQMF.findAllInForIssueReportId issueReport.mediaFiles issueReportId
  comments <- Esq.runInReplica (QC.findAllByIssueReportId issueReport.id)
  category <- CQIC.findById issueReport.categoryId >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
  driverDetail <- mapM mkDriverDetail =<< Esq.runInReplica (QP.findById issueReport.driverId)
  option <- mapM (\optionId -> CQIO.findById optionId >>= fromMaybeM (IssueOptionNotFound optionId.getId)) issueReport.optionId
  pure $
    Common.IssueInfoRes
      { issueReportId = cast issueReport.id,
        driverDetail,
        rideId = cast <$> issueReport.rideId,
        category = category.category,
        option = option <&> (.option),
        mediaFiles = mkCommonMediaFiles mediaFiles,
        comments = mkIssueReportComment <$> comments,
        description = issueReport.description,
        assignee = issueReport.assignee,
        status = toCommonIssueStatus issueReport.status,
        createdAt = issueReport.createdAt
      }
  where
    mkDriverDetail :: DP.Person -> Flow Common.DriverDetail
    mkDriverDetail driverDetail = do
      mobileNumber <- traverse decrypt driverDetail.mobileNumber
      pure $
        Common.DriverDetail
          { driverId = cast driverDetail.id,
            firstName = driverDetail.firstName,
            middleName = driverDetail.middleName,
            lastName = driverDetail.lastName,
            mobileNumber
          }
    mkCommonMediaFiles :: [DMF.MediaFile] -> [Common.MediaFile]
    mkCommonMediaFiles =
      foldr'
        ( \mediaFile commonMediaFileList -> do
            case mediaFile._type of
              DMF.Audio -> Common.MediaFile Common.Audio mediaFile.url : commonMediaFileList
              DMF.Image -> Common.MediaFile Common.Image mediaFile.url : commonMediaFileList
              _ -> commonMediaFileList
        )
        []
    mkAuthorDetails :: Id DP.Person -> Common.AuthorDetail
    mkAuthorDetails authorId =
      Common.AuthorDetail
        { authorId = cast authorId,
          firstName = Nothing,
          lastName = Nothing
        }
    mkIssueReportComment :: DC.Comment -> Common.IssueReportCommentItem
    mkIssueReportComment comment =
      Common.IssueReportCommentItem
        { comment = comment.comment,
          authorDetail = mkAuthorDetails comment.authorId,
          timestamp = comment.createdAt
        }

issueUpdate :: ShortId DM.Merchant -> Id DIR.IssueReport -> Common.IssueUpdateByUserReq -> Flow APISuccess
issueUpdate _merchantShortId issueReportId req = do
  unless (isJust req.status || isJust req.assignee) $
    throwError $ InvalidRequest "Empty request, no fields to update."
  void $ CQIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  Esq.runTransaction $ QIR.updateStatusAssignee issueReportId (toDomainIssueStatus <$> req.status) req.assignee
  whenJust req.assignee mkIssueAssigneeUpdateComment
  CQIR.invalidateIssueReportCache (Just issueReportId) Nothing
  pure Success
  where
    mkIssueAssigneeUpdateComment assignee = do
      id <- generateGUID
      now <- getCurrentTime
      Esq.runTransaction $
        QC.create $
          DC.Comment
            { id,
              issueReportId,
              authorId = cast req.userId,
              comment = "Assignee Updated : " <> show assignee,
              createdAt = now
            }

issueAddComment :: ShortId DM.Merchant -> Id DIR.IssueReport -> Common.IssueAddCommentByUserReq -> Flow APISuccess
issueAddComment _merchantShortId issueReportId req = do
  void $ CQIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  Esq.runTransaction $ QC.create =<< mkComment
  pure Success
  where
    mkComment = do
      id <- generateGUID
      now <- getCurrentTime
      pure $
        DC.Comment
          { id,
            issueReportId,
            comment = req.comment,
            authorId = cast req.userId,
            createdAt = now
          }
