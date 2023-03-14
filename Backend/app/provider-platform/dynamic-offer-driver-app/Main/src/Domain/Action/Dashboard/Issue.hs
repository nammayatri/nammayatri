module Domain.Action.Dashboard.Issue where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Message.MediaFile as DMF
import qualified Domain.Types.Issue.IssueReport as DIR
import qualified Domain.Types.Issue.Comment as DC
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Issue as Common
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess(Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Issue.IssueReport as QIR
import qualified Storage.Queries.Issue.Comment as QC
import qualified Storage.Queries.Message.MediaFile as QMF
import Tools.Error

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

issueList :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Common.IssueStatus -> Maybe Text -> Maybe Text -> Flow Common.IssueReportListResponse
issueList _merchantShortId mbLimit mbOffset mbStatus _mbCategory _mbOption = do
  issueReports <- Esq.runInReplica $ QIR.findAllWithLimitOffsetStatus mbLimit mbOffset (toDomainIssueStatus <$> mbStatus)
  let count = length issueReports
  let summary = Common.Summary { totalCount = count, count }
  issues <- mapM mkIssueReport issueReports
  return $ Common.IssueReportListResponse { issues, summary }
  where
    mkIssueReportComment :: DC.Comment -> Common.IssueReportCommentItem
    mkIssueReportComment DC.Comment {..} =
      Common.IssueReportCommentItem
        { timestamp = createdAt,
          ..
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
    mkIssueReport :: DIR.IssueReport -> Flow Common.IssueReportListItem
    mkIssueReport issueReport = do
      comments <- Esq.runInReplica $ QC.findAllByIssueReportId issueReport.id
      mediaFiles <- Esq.runInReplica $ QMF.findAllIn issueReport.mediaFiles
      pure $
        Common.IssueReportListItem
          { issueReportId = cast issueReport.id,
            driverId = cast issueReport.driverId,
            rideId = cast <$> issueReport.rideId,
            category = issueReport.category,
            option = issueReport.option,
            assignee = issueReport.assignee,
            description = issueReport.description,
            status = toCommonIssueStatus issueReport.status,
            comments = mkIssueReportComment <$> comments,
            mediaFiles = mkCommonMediaFiles mediaFiles
          }

issueUpdate :: ShortId DM.Merchant -> Id DIR.IssueReport -> Common.IssueUpdateReq -> Flow APISuccess
issueUpdate _merchantShortId issueReportId Common.IssueUpdateReq {..} = do
  case (status, assignee) of
    (Just justStatus, Just justAssignee) -> do
      Esq.runTransaction $ QIR.updateStatusAssignee issueReportId (toDomainIssueStatus justStatus) justAssignee
      pure Success
    (Just justStatus, _) -> do
      Esq.runTransaction $ QIR.updateStatus issueReportId (toDomainIssueStatus justStatus)
      pure Success
    (_, Just justAssignee) -> do
      Esq.runTransaction $ QIR.updateAssignee issueReportId justAssignee
      pure Success
    (Nothing, Nothing) -> throwError $ InvalidRequest "Empty request, no fields to update."
      
issueAddComment :: ShortId DM.Merchant -> Id DIR.IssueReport -> Common.IssueAddCommentReq -> Flow APISuccess
issueAddComment _merchantShortId issueReportId Common.IssueAddCommentReq {..} = do
  Esq.runTransaction $ QC.create =<< mkComment
  pure Success
  where
    mkComment = do
      id <- generateGUID
      now <- getCurrentTime
      pure $
        DC.Comment
          { createdAt = now,
            ..
          }
